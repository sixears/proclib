{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module ProcLib.Wc
  ( AsWcError( _WcError ), WcError, WcParseLine(..), WcParseError(..)
  , lineCount, lineCount', lineCount1, lineCount1'
  , wc, wc', wcCmd, wcCmd', wcFilename, wcTxtOpts, wcParse
  )
where

import Prelude ( )

-- base --------------------------------

import Control.Exception       ( Exception )
import Control.Monad           ( (>>=), mapM, return, sequence, when )
import Data.Either             ( Either( Left, Right ) )
import Data.Eq                 ( Eq, (/=) )
import Data.Foldable           ( Foldable, foldr, null, toList )
import Data.Function           ( (.), ($), (&), const )
import Data.Functor            ( fmap, (<$>) )
import Data.List               ( concat, filter, sort, zip )
import Data.List.NonEmpty      ( NonEmpty, fromList, init, last )
import Data.Maybe              ( Maybe( Nothing ) )
import Data.Monoid             ( (<>) )
import Data.Ord                ( (>) )
import Data.Tuple              ( fst, snd )
import Numeric.Natural         ( Natural )
import Prelude                 ( (+) )
import Text.Show               ( Show )

-- fluffy ------------------------------

import Fluffy.Foldable    ( length )
import Fluffy.Maybe       ( (<^*?) )
import Fluffy.MonadError  ( mapMError )
import Fluffy.Path        ( AbsFile, parseAbsFile )
import Fluffy.Path.Error  ( PathError )
import Fluffy.Text        ( nSplitWords )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( (^.) )
import Control.Lens.Prism   ( prism' )
import Control.Lens.Review  ( (#) )
import Control.Lens.Setter  ( (?~), (%~) )
import Control.Lens.TH      ( makeClassyPrisms, makeLenses )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable, otoList )
import Data.NonNull          ( NonNull, fromNonEmpty, ncons )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )
import Control.Monad.Trans   ( lift )

-- path --------------------------------

import Path  ( toFilePath )

-- text --------------------------------

import Data.Text       ( Text, lines, pack )
import Data.Text.Read  ( decimal )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Wc.Opt   as  Wc
import qualified  ProcLib.Wc.Req   as  Req

import ProcLib.CommonOpt.Busybox      ( AsBusyboxOpt, boolBBOpt )
import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError )
import ProcLib.Error.ExecError        ( errorIfNonZero )
import ProcLib.Wc.Opt                 ( AsWcOpt, WcOpt, wcOpts )
import ProcLib.Wc.Req                 ( HasWcReq )
import ProcLib.Process                ( ExitVal, mkProc )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ), cmdArgs )
import ProcLib.Types.ProcIO           ( ProcIO )
import ProcLib.Wc.Req                 ( WcReq )

-------------------------------------------------------------------------------

data WcParseLine = WcParseLine { _wcFilename      :: AbsFile
                               , _wcBytes         :: Maybe Natural
                               , _wcChars         :: Maybe Natural
                               , _wcWords         :: Maybe Natural
                               , _wcLines         :: Maybe Natural
                               , _wcMaxLineLength :: Maybe Natural
                               }
  deriving (Eq, Show)

$( makeLenses ''WcParseLine )

defWcParseLine :: AbsFile -> WcParseLine
defWcParseLine fn = WcParseLine fn Nothing Nothing Nothing Nothing Nothing

----------------------------------------

data WcParseError = WcParseErrorT Text -- error description
                                  Text -- offending line
                  | WcParsePathError PathError
  deriving (Eq, Show)

$( makeClassyPrisms ''WcParseError )

mkWcParseError :: AsWcParseError ?? => Text -> Text -> ??
mkWcParseError desc line = _WcParseError # WcParseErrorT desc line

------------------------------------------------------------

data WcError = WcExitE   CmdSpec ExitVal Text Text
             | WcParseE  WcParseError
             | WcCreateE CreateProcError
  deriving (Eq, Show)

$( makeClassyPrisms ''WcError )

instance Exception WcError

instance AsCreateProcError WcError where
  _CreateProcError = prism' WcCreateE (^? _WcCreateE)

mkWcExitE :: AsWcError ?? => CmdSpec -> ExitVal -> (Text,Text) -> ??
mkWcExitE cmd ev (sout,serr) = _WcError # WcExitE cmd ev sout serr

mkWcParseE :: AsWcError ?? => WcParseError ->  ??
mkWcParseE pe = _WcError # WcParseE pe

mkWcParseError' :: AsWcError ?? => Text -> Text -> ??
mkWcParseError' desc line = mkWcParseE (mkWcParseError desc line)

------------------------------------------------------------

readNat :: (AsWcParseError ??, MonadError ?? ??) => Text -> Text -> ?? Natural
readNat l t =
  let chuff x i' = [fmt|found chuff '%t' at end of '%t' (after %d)|] x t i'
   in case decimal t of
             Left  s      -> throwError $ mkWcParseError ([fmt|%s: %t|] s t) l
             Right (i,"") -> return i
             Right (i,x)  -> throwError $ mkWcParseError (chuff x i) l

wcParseLine :: MonadError WcParseError ?? => WcReq -> Text -> ?? WcParseLine
wcParseLine req l = do
  let -- for correct operation, colLenses must much the order that wc prints
      -- its columns in (which is a static order)
      colLenses = fmap snd $
                    filter ((req ^.) . fst)
                           [ (Req.wcLines, wcLines)
                           , (Req.wcWords, wcWords)
                           , (Req.wcChars, wcChars)
                           , (Req.wcBytes, wcBytes)
                           , (Req.wcMaxLineLength, wcMaxLineLength)
                           ]
      words = nSplitWords (1 + length colLenses) l
      (cols, filename) = (init words, last words)
  let wrongColMsg = [fmt|wrong column count (got %d; expected %d)|]
                                           (length cols) (length colLenses)
  when -- should never happen, if nSplitWords is written right (because then
       -- you may end up with empty columns, but never blank ones)
       (length cols /= length colLenses)
       (throwError $ mkWcParseError wrongColMsg l)
  colVals <- sequence $ fmap (readNat l) cols
  fn <- case parseAbsFile filename of
          Right fn -> return fn
          Left  p  -> throwError $ WcParsePathError p
  return $ foldr (\ (lens,val) res -> res & lens ?~ val) (defWcParseLine fn)
                 (zip colLenses colVals)

wcParse :: MonadError WcParseError ?? => WcReq -> Text -> ?? [WcParseLine]
wcParse req txt =
  let ls = lines txt
      -- if there are multiple files, then wc prints a total at the end, which
      -- we will not parse
      ls' = if length ls > 1 then init (fromList ls) else ls
   in sequence $ fmap (wcParseLine req) ls'

wcParseErrorAsWcError :: (AsWcError ??, MonadError ?? ??) =>
                         Either WcParseError ?? -> ?? ??
wcParseErrorAsWcError = mapMError ((_WcError #) . WcParseE)

wcParse' :: (AsWcError ??, MonadError ?? ??) => WcReq -> Text -> ?? [WcParseLine]
wcParse' req txt = wcParseErrorAsWcError (wcParse req txt)

------------------------------------------------------------

wcTxtOpts :: HasWcReq ?? => ?? -> CmdSpec
wcTxtOpts req =
  CmdSpec (req ^. Req.wcPath)
          (sort (concat [ mebbe Req.wcBytes          "--bytes"            "-c"
                        , mebbe Req.wcChars          "--chars"            "-m"
                        , mebbe Req.wcLines          "--lines"            "-l"
                        , mebbe Req.wcWords          "--words"            "-w"
                        , mebbe Req.wcMaxLineLength  "--max-line-length"  "-L"
                        ])
          )
  where mebbe  = boolBBOpt req Req.wcBusyboxOpts

----------------------------------------

wcTxt :: (Element ?? ~ AbsFile, MonoFoldable ??, HasWcReq ??) =>
         ?? -> NonNull ?? -> CmdSpec
wcTxt req fns =
  wcTxtOpts req & cmdArgs %~ (<> ("--" : fmap (pack . toFilePath) (otoList fns)))

-------------------------------------------------------------------------------

-- | default to Wc.lines, to ensure consistency (rather than relying on
--   wc's own default) and it's my most common usage
wcOptsDef :: (AsWcOpt ??, AsBusyboxOpt ??, Foldable ??) => ?? ?? -> WcReq
wcOptsDef opts =
  if null opts then wcOpts [Wc.lines :: WcOpt] else wcOpts $ toList opts

wcCmd :: (AsWcOpt ??, AsBusyboxOpt ??, Element ?? ~ AbsFile, MonoFoldable ??,
          Foldable ??) =>
         NonNull ?? -> ?? ?? -> CmdSpec
wcCmd fns opts = wcTxt (wcOptsDef opts) fns

wcCmd' :: Foldable ?? => NonEmpty AbsFile -> ?? WcOpt -> CmdSpec
wcCmd' fns = wcCmd ((fromNonEmpty fns) :: NonNull [AbsFile])

-------------------------------------------------------------------------------

-- | run a wc, return the parsed lines and any stderr emitted
wc :: (MonoFoldable ??, Element ?? ~ AbsFile,
       Foldable ??, AsWcOpt ??, AsBusyboxOpt ??,
       AsWcError ??, AsCreateProcError ??, MonadError ?? ??) =>
      NonNull ?? -> ?? ?? -> ProcIO ?? ?? ([WcParseLine], Text)
wc fns opts = do
  let req   = wcOptsDef opts
      cspec = wcTxt req fns
  (exitval, (stdout,stderr)) <- mkProc cspec
  _ <- lift $ errorIfNonZero mkWcExitE cspec (exitval, (stdout,stderr))
  parsedLines <- return stdout >>= lift . wcParse' req
  return (parsedLines, stderr)

----------------------------------------

wc' :: (MonoFoldable ??, Element ?? ~ AbsFile, Foldable ??, MonadError WcError ??) =>
       NonNull ?? -> ?? WcOpt -> ProcIO WcError ?? ([WcParseLine], Text)
wc' = wc

----------------------------------------

-- | pull a lines count from a WcParse, throw error if not present
parseToLines :: (AsWcError ??, MonadError ?? ??) => WcParseLine -> ?? Natural
parseToLines = wcLines <^*? (const $ mkWcParseError' "no lines" "")

-- | pull a lines count and filename from a WcParseLine
parseToFnLines :: (AsWcError ??, MonadError ?? ??) =>
                  WcParseLine -> ?? (AbsFile, Natural)
parseToFnLines p = (p ^. wcFilename,) <$> parseToLines p

----------------------------------------

lineCount :: (MonoFoldable ??, Element ?? ~ AbsFile,
              AsWcError ??, AsCreateProcError ??, MonadError ?? ??) =>
             NonNull ?? -> ProcIO ?? ?? ([(AbsFile, Natural)], Text)
lineCount fns = do
  (parsedLines, err) <- wc fns [Wc.lines :: WcOpt]
  lineCounts <- lift $ mapM parseToFnLines parsedLines
  return (lineCounts, err)

----------------------------------------

lineCount' :: (Element ?? ~ AbsFile, MonoFoldable ??, MonadError WcError ??) =>
              NonNull ?? -> ProcIO WcError ?? ([(AbsFile, Natural)], Text)
lineCount' = lineCount

----------------------------------------

lineCount1 :: (AsWcError ??, AsCreateProcError ??, MonadError ?? ??) =>
              AbsFile -> ProcIO ?? ?? (Natural, Text)
lineCount1 fn = do
  ~([(_, n)],err) <- lineCount (fn `ncons` [])
  return (n,err)

----------------------------------------

lineCount1' :: MonadError WcError ?? =>
               AbsFile -> ProcIO WcError ?? (Natural, Text)
lineCount1' = lineCount1

-- that's all, folks! ---------------------------------------------------------

