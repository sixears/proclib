{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}

module ProcLib.T.Process
  ( assertIOException, assertLeft, tests )
where

-- base --------------------------------

import Control.Monad           ( (>>=) {- , mapM -}, return {- , sequence -} )
-- import Control.Monad.IO.Class  ( MonadIO, liftIO )
-- import Data.Bool               ( Bool( True ) )
import Data.Either             ( Either( Left, Right ) )
import Data.Eq                 ( Eq {- , (==) -} )
-- import Data.Foldable           ( concat )
import Data.Function           ( ($) {- , (&) -} )
-- import Data.Functor            ( (<$>), fmap )
-- import Data.List               ( filter )
-- import Data.Maybe              ( Maybe( Just, Nothing ) )
-- import Data.Monoid             ( (<>), mconcat )
import Data.String             ( String )
-- import Data.Traversable        ( traverse )
-- import Data.Tuple              ( uncurry )
import Data.Word               ( Word8 )
-- import Numeric.Natural         ( Natural )
import System.IO               ( IO {- , hClose -} )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

-- import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

-- import Data.Default  ( def )

-- fluffy ------------------------------

-- import FPath.PathComponent  ( pc )
import MonadError.IO.Error       ( AsIOError )
-- import MonadError     ( splitMError )
import MonadError.IO  ( asIOError )
import Natural            ( {- AtMost( Nil ), -} Two )
import TastyPlus          ( {- assertRight, -} runTestsP_ )
-- import MonadIO.Temp       ( tempfile' )

-- lens --------------------------------

-- import Control.Lens.Fold    ( (^?) )
-- import Control.Lens.Getter  ( (^.) )
-- import Control.Lens.Setter  ( (.~), (?~) )
import Control.Lens.TH      ( makeLenses )

-- mtl ---------------------------------

import Control.Monad.Except    ( {- ExceptT, MonadError, -} runExceptT )
-- import Control.Monad.Identity  ( Identity, runIdentity )
-- import Control.Monad.Trans     ( lift )

-- path --------------------------------

-- import Path  ( Abs, File, Path, absdir, absfile, mkAbsFile, toFilePath )

-- streaming ---------------------------

-- import qualified  Streaming.Prelude  as  S
-- import Streaming.Prelude  ( lazily )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion --, (@?=)
                         -- , assertBool, assertEqual
                         , assertFailure
                         -- , testCase
                         )

-- text --------------------------------

import Data.Text     ( Text
                     -- , empty, isInfixOf, lines, pack, toLower, unlines, unpack
                     )
-- import Data.Text.IO  ( readFile, writeFile )

-- unix --------------------------------

-- import System.Posix.Files  ( removeLink )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

-- import qualified  ProcLib.Paths  as  Paths

import ProcLib.CommonOpt.DryRun       ( DryRunLevel
                                      , HasDryRunLevel( dryRunLevel )
                                      -- , dryRunOff, dryRunOn
                                      )
import ProcLib.CommonOpt.Verbose      ( VerboseLevel -- ( VerboseLevel )
                                      , HasVerboseLevel( verboseLevel ) )
{-
import ProcLib.Error.CreateProcError  ( AsCreateProcError, CreateProcError )
import ProcLib.Error.ExecCreateError  ( ExecCreateError, ExecCreatePathIOError
                                      , _ECCreateE, _ECExecE, ecExecE' )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ), ToMaybeTexts
                                      , cmdspec, exitval, stderr, stdout )
import ProcLib.Process                ( ExitVal( ExitVal )
                                      , mkExec, mkIO, mkProc, mkProc', mkProc_
                                      , mkProc'_, doProcIO
                                      , mockProcIO, runProcIO, runProcIO'
                                      )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.CreateProcOpts   ( CreateProcOpts
                                      , cwd, defCPOpts, mock, mockLvl, withInT )
import ProcLib.Types.Handles          ( Handles )
import ProcLib.Types.MakeProc         ( MakeProc )
import ProcLib.Types.MockDefault      ( MockDefault )
import ProcLib.Types.ProcExecCtxt     ( Exited )
import ProcLib.Types.ProcIO           ( ProcIO, ProcIO' )
import ProcLib.Types.ProcIOAction     ( ProcIOAction( DoCmd, DoMockCmd ) )
import ProcLib.Types.RunProcOpts      ( defOpts, dryRunL, verboseL )
-}
-------------------------------------------------------------------------------

{-
myGrep ∷ (Text, [Path Abs File]) → IO [Text]
myGrep (t, fns) = do
  ls ← concat <$> (fmap lines <$> (mapM (readFile ∘ toFilePath) fns))
  return $ filter (t `isInfixOf`) ls
-}

newtype MyError = MyError (Word8, Text, Text)
  deriving (Eq, Show)


-- | I got a 'Left' value, that matches some assertion predicate
assertLeft ∷ Show ρ ⇒ (γ → Assertion) → Either γ ρ → Assertion
assertLeft assertion got =
  case got of Right r → assertFailure (show r)
              Left  l → assertion l

assertIOException ∷ (AsIOError ε, Show ρ) ⇒
                     (ε → Assertion) → IO ρ → Assertion
assertIOException p io = (runExceptT $ asIOError io) >>= \e → assertLeft p e

{-
assertIOException_ ∷ Show ρ ⇒
                      (γ → Assertion) → IO (Either γ ρ) → Assertion
assertIOException_ p io = io >>= \e → assertLeft p e

isNoSuchFileEx ∷ Show α ⇒ α → Assertion
isNoSuchFileEx e = assertBool (show e) (txt `isInfixOf` (toLowerT e))
                   where txt      = "no such file or directory"
                         toLowerT = toLower ∘ pack ∘ show
-}
----------------------------------------

{-
runProc ∷ (MonadIO μ, MonadError ε η) ⇒ ProcIO' ε (ExceptT ε μ) ω → μ (η ω)
runProc = splitMError ∘ S.effects ∘ runProcIO' defOpts

runProcM ∷ MonadError ε η ⇒ ProcIO' ε (ExceptT ε Identity) ω → η ω
runProcM = runIdentity ∘ splitMError ∘ S.effects ∘ mockProcIO

ooProc ∷ (MockDefault ω, Handles ζ ω, MakeProc ζ, ToMaybeTexts ω,
           AsCreateProcError ε, AsExecError ε, MonadError ε η, MonadIO μ) ⇒
          CreateProcOpts (ExitVal, ω) → CmdSpec → μ (η ω)
ooProc o = runProc ∘ mkProc'_ o

ooProcM ∷ (MockDefault ω, Handles ζ ω, MakeProc ζ, ToMaybeTexts ω,
           AsCreateProcError ε, AsExecError ε, MonadError ε η) ⇒
          CreateProcOpts (ExitVal, ω) → CmdSpec → η ω
ooProcM o = runProcM ∘ mkProc'_ o

ooProcTT ∷ CreateProcOpts (ExitVal, (Text,Text)) → CmdSpec
         → IO (Either ExecCreateError (Text,Text))
ooProcTT = ooProc

goProc ∷ (MockDefault ω, Handles ζ ω, MakeProc ζ, ToMaybeTexts ω,
           AsCreateProcError ε, AsExecError ε, MonadError ε η, MonadIO μ) ⇒
          CmdSpec → μ (η ω)
goProc = ooProc def

goProcM ∷ (MockDefault ω, Handles ζ ω, MakeProc ζ, ToMaybeTexts ω,
           AsCreateProcError ε, AsExecError ε, MonadError ε η) ⇒
          CmdSpec → η ω
goProcM = ooProcM def

goProc' ∷ (MockDefault ω, Handles ζ ω, MakeProc ζ, ToMaybeTexts ω) ⇒
           CmdSpec → IO (Either ExecCreateError ω)
goProc' = goProc

goProc_ ∷ CmdSpec → IO (Either ExecCreateError ())
goProc_ = goProc

goProcTT ∷ CmdSpec → IO (Either ExecCreateError (Text,Text))
goProcTT = goProc

goProcMTT ∷ CmdSpec → Either ExecCreateError (Text,Text)
goProcMTT = goProcM

toECreateE ∷ α → Either ExecCreateError α
toECreateE a = Right a

toECreatePIOE ∷ α → Either ExecCreatePathIOError α
toECreatePIOE a = Right a

etcPasswd ∷ Path Abs File
etcPasswd = [absfile|/etc/passwd|]

etcGroup ∷ Path Abs File
etcGroup  = [absfile|/etc/group|]
-}

systemTests ∷ TestTree
systemTests =
  let -- trueCmd    = CmdSpec Paths.true    []
--      falseCmd   = CmdSpec Paths.false   []
--      true       ∷ IO (Either ExecCreateError ())
--      true       = goProc' trueCmd
--      false      ∷ IO (Either ExecCreateError ())
--      false      = goProc' falseCmd
--      nonsuch    ∷ Path Abs File
--      nonsuch    =  $( mkAbsFile "/bin/nonsuch" )
--      nonsuchCmd ∷ CmdSpec
--      nonsuchCmd = CmdSpec nonsuch []
   in testGroup "system"
       [ {- testCase "true" $ true >>= (@?= Right ())
       , testCase "false" $
             false >>= (@?= Left (ecExecE' falseCmd (ExitVal 1) ()))
       , testCase "nonsuch" $
             assertIOException_ isNoSuchFileEx $ goProc_ nonsuchCmd
       , testCase "true mock" $
                 (lazily <$> (S.toList $ mockProcIO (mkProc_ trueCmd)))
             @?= toECreateE ([DoMockCmd trueCmd],())
       , testCase "false mock" $
                 (lazily <$> (S.toList $ mockProcIO (mkProc_ falseCmd)))
             @?= toECreateE ([DoMockCmd falseCmd],())
       , testCase "nonsuch mock" $
                 splitMError
                     (runProcIO (defOpts & dryRunL .~ 1) (mkProc_ nonsuchCmd))
             >>= (@?= toECreateE ())
-}
       ]

----------------------------------------

{-
grepC ∷ Text → [Path Abs File] → CmdSpec
grepC pat fns = CmdSpec Paths.grep (pat : fmap (pack ∘ toFilePath) fns)
-}

captureTests ∷ TestTree
captureTests =
  let -- input = "bar\nfoo\nbaz\n" ∷ Text

{-
      checkGrep (txt,fn) = do
        grepped  ← unlines <$> myGrep (txt,[fn])
        captured ← goProcTT (CmdSpec Paths.grep [ txt, pack $ toFilePath fn ])
        captured @?= Right (grepped, "")

      checkGrepF (txt,fn) = do
        let cspec = CmdSpec Paths.grep [ txt, pack fn ]
        captured ← goProcTT cspec
        captured @?= Left (ecExecE' cspec (ExitVal 1) (empty,empty))

      -- | run a sequence of captures, accumulate the results
      concatGreps ∷ [(Text, [Path Abs File])]
                  → IO (Either ExecCreateError (Text,Text))
      concatGreps patPaths =
        fmap (fmap mconcat) $
          sequence <$> traverse (goProcTT ∘ uncurry grepC) patPaths

      -- | run a sequence of captures, accumulate the results
      concatGrepsM ∷ [(Text, [Path Abs File])]
                  → Either ExecCreateError (Text,Text)
      concatGrepsM patPaths =
        fmap (fmap mconcat) $
          sequence <$> traverse (goProcMTT ∘ uncurry grepC) patPaths

      checkGreps cmds = do
        grepped  ← fmap (unlines ∘ concat) (mapM myGrep cmds)
        captured ← concatGreps cmds
        captured @?= Right (grepped, "")

      checkGrepsF cmds (xpat, xfns) = do
        captured ← concatGreps cmds
        captured
            @?= Left (ecExecE' (CmdSpec Paths.grep
                                 (xpat : (pack ∘ toFilePath <$> xfns)))
                               (ExitVal 1)
                               (empty,empty)
                     )
      checkMockGreps cmds = concatGrepsM cmds @?= toECreateE (empty,empty)
-}

   in testGroup "capture"
       [ {- testCase "pwd, with dir" $
             ooProcTT (def & cwd ?~ [absdir|/|]) (CmdSpec Paths.pwd [])
               >>= (@?= toECreateE ("/\n",""))
       , testCase "pwd, with dir (force mock)" $
             ooProcTT (def & mockLvl .~ 0 & cwd ?~ [absdir|/|])
                      (CmdSpec Paths.pwd [])
               >>= (@?= toECreateE ("",""))
       , testCase "pwd, with bad dir" $
             assertIOException_ isNoSuchFileEx
                 (ooProcTT (def & cwd ?~ [absdir|/nosuchdir|])
                           (CmdSpec Paths.pwd []))
       , testCase "pwd, with bad dir (force mock)" $
             ooProcTT (def & mockLvl .~ 0 & cwd ?~ [absdir|/nosuchdir|])
                      (CmdSpec Paths.pwd [])
               >>= (@?= toECreateE ("",""))
         testCase "grep root /etc/passwd" $
             checkGrep ("root", etcPasswd)
       , testCase "grep nosuchname /etc/passwd" $
             checkGrepF ("nosuchname", "/etc/passwd")
         testCase "grep root /etc/passwd (seq)" $
             checkGreps [("root", [etcPasswd])]
       , testCase "grep root /etc/passwd, /etc/group" $
             checkGreps [("root",[etcPasswd]),("root",[etcGroup])]
       , testCase "grep root /etc/group, /etc/passwd" $
             checkGreps [("root",[etcGroup]),("root",[etcPasswd])]
       , testCase "grep root /etc/passwd, nosuchword /etc/passwd " $
             checkGrepsF [("root",[etcPasswd]),("nosuchword",[etcPasswd])]
                         ("nosuchword",[etcPasswd])
       , testCase "grep nosuchword /etc/passwd, root /etc/passwd " $
             checkGrepsF [("nosuchword",[etcPasswd]),("root",[etcPasswd])]
                         ("nosuchword",[etcPasswd])
       , testCase "grep root /etc/passwd (seq)" $
             checkMockGreps [("root", [etcPasswd])]
       , testCase "grep root /etc/passwd, /etc/group" $
             checkMockGreps [ ("root",[etcPasswd])
                            , ("root",[etcGroup]) ]
       , testCase "grep root /etc/group, /etc/passwd " $
             checkMockGreps [ ("root",[etcGroup])
                            , ("root",[etcPasswd]) ]
       , testCase "mock grep nosuchword /etc/passwd, root /etc/passwd " $
             checkMockGreps [ ("nosuchword",[etcPasswd])
                            , ("root",[etcPasswd]) ]
       , testCase "fed grep" $
           (ooProcTT (def `withInT` input) (CmdSpec Paths.grep ["ba"])) >>=
               (@?= toECreateE ("bar\nbaz\n",""))

       , testCase "fed grep (no match)" $ do
           let cspec = CmdSpec Paths.grep ["xx"]
               opts  = (def `withInT` input)
           r ← ooProcTT opts cspec
           r @?= (Left (ecExecE' cspec (ExitVal 1) (empty,empty)))

       , testCase "fed grep (no file; capture stderr)" $ do
           let c = CmdSpec Paths.grep ["xx", "/nonsuch"]
               check e = do case e ^? _ECExecE of
                              Nothing → assertFailure (show e)
                              Just e' → do
                                e' ^. cmdspec @?= c
                                e' ^. exitval @?= ExitVal 2
                                e' ^. stdout  @?= Just empty
                                case e' ^. stderr of
                                  Nothing → assertFailure "no stderr"
                                  Just s  → assertBool (unpack s)
                                                        ("No such file"
                                                         `isInfixOf`s)
           result ← ooProcTT (def `withInT` input) c
           assertLeft check result

       , testCase "fed grep (no file; capture stderr; bad directory)" $ do
           let c = CmdSpec Paths.grep ["xx", "/nonsuch"]
               check e = do case e ^? _ECCreateE of
                              Nothing → assertFailure (show e)
                              Just e' → do
                                isNoSuchFileEx e'
                                assertBool (show e')
                                           ("chdir: does not exist" `isInfixOf`
                                               (pack $ show e'))
           result ← ooProcTT (def `withInT` input
                                   & cwd ?~ [absdir|/nosuchdir|]) c
           assertLeft check result
-}
       ]


----------------------------------------

-- Some IO for testing, run 'true' and grep root /etc/passwd as controlled
-- procs, as well as some real IO (that is, creating and then removing a temp
-- file); but under controlled IO, write some text to that temp file (thus, in
-- mock mode, don't write that).  Return stdout,stderr (from 'grep'), and the
-- contents of the temp file.
{-
testIO ∷ ProcIO' ExecCreatePathIOError (ExceptT ExecCreatePathIOError IO)
                                       ((Text,Text), Text)

testIO = do
  () ← mkProc_ (CmdSpec Paths.true [])
  (tempfn, tempfh) ← lift $ tempfile' [pc|proctest|] ()
  () ← lift ∘ liftIO $ hClose tempfh
  let writeTxt = "cyborg\n\n"
  mkIO "temp file" (liftIO $ writeFile (toFilePath tempfn) writeTxt)
  gotTxt ∷ Text ← lift ∘ liftIO $ readFile (toFilePath tempfn)
  cap ∷ (Text,Text) ← mkProc_ (CmdSpec Paths.grep
                                         [ "root", pack (toFilePath etcPasswd) ])
  let r ∷ ((Text,Text),Text) = (cap,gotTxt)
  () ← lift ∘ liftIO $ removeLink (toFilePath tempfn)
  return r
-}

monadTests ∷ TestTree
monadTests =
  testGroup "monad (sytem & capture, with >>)"
    [ {- testCase "systemx >> capture" $ do
        grepped   ← unlines <$> myGrep ("root",[etcPasswd])
        capgotten ← runProc testIO
        capgotten @?= toECreatePIOE ((grepped, empty),"cyborg\n\n")
    , testCase "systemx >> capture (mock)" $ do
        capgotten ← runExceptT ∘ S.effects ∘ mockProcIO $ testIO
        capgotten @?= toECreatePIOE ((empty, empty),empty)
    , testCase "capture >> systemx" $ do
        captured ← runProc $ do
          _ ∷ Text ← mkProc_ (CmdSpec Paths.grep
                                        [ "root"
                                        , pack (toFilePath etcPasswd) ])
          mkProc_ (CmdSpec Paths.true [])
        captured @?= toECreateE ()
    , testCase "capture >> systemx (r)" $ do
        grepped  ← unlines <$> myGrep ("root", [etcPasswd])
        captured ← runProc $ do
          r ← mkProc_ (CmdSpec Paths.grep [ "root"
                                           , pack (toFilePath etcPasswd) ])
          () ← mkProc_ (CmdSpec Paths.true [])
          return r
        captured @?= toECreateE (grepped,empty)
    , testCase "capture' >> capture" $ do
        grepped  ← unlines <$> myGrep ("root",[etcPasswd])
        captured ← runProc $ do
          let input = "foo\nroot\nbaz" ∷ Text
          ~(rs,"" ∷ Text) ← mkProc'_ (defCPOpts `withInT` input)
                                      (CmdSpec Paths.grep ["r"])
          let [r] = lines rs
          mkProc_ (CmdSpec Paths.grep [ r, pack (toFilePath etcPasswd) ])
        captured @?= toECreateE (grepped,empty)
-}
    ]

----------------------------------------

data MyOption = MyOption { _dryRunLvl  ∷ DryRunLevel  Two
                         , _verboseLvl ∷ VerboseLevel Two }

$( makeLenses ''MyOption )

instance HasDryRunLevel Two MyOption where
  dryRunLevel = dryRunLvl

instance HasVerboseLevel Two MyOption where
  verboseLevel = verboseLvl

{-
trueC ∷ CmdSpec
trueC = CmdSpec Paths.true []

falseC ∷ CmdSpec
falseC = CmdSpec Paths.false []

mock1T ∷ CreateProcOpts (ExitVal, Text)
mock1T = def & mock .~ (ExitVal 1, "alpha")

mock0TT ∷ CreateProcOpts (ExitVal, (Text,Text))
mock0TT = def & mock .~ (ExitVal 0, ("delta","epsilon"))

mock1TT ∷ CreateProcOpts (ExitVal, (Text,Text))
mock1TT = def & mock .~ (ExitVal 1, ("beta","gamma"))

mockTT ∷ (Text,Text)
mockTT = ("","")

mockT ∷ Text
mockT = ""
-}

{-
mockEchoFoo ∷ Either ExecCreateError ([ProcIOAction], (ExitVal, Text))
mockEchoFoo = lazily <$> (S.toList $ (mockProcIO $ mkProc echoFoo))

mockEchoFoo1 ∷ Either ExecCreateError ([ProcIOAction], (ExitVal, Text))
mockEchoFoo1 = lazily <$> (S.toList $ (mockProcIO $ mkProc' mock1T echoFoo))

mockEchoFooFail ∷ Either ExecCreateError ([ProcIOAction], Text)
mockEchoFooFail = lazily <$> (S.toList $ mockProcIO $ mkProc'_ mock1T echoFoo)

runEchoFoo ∷ IO (Either ExecCreateError ([ProcIOAction], (ExitVal, Text)))
runEchoFoo = splitMError $
  lazily <$> (S.toList $ (runProcIO' defOpts $ mkProc (echoC "foo")))



mockTrue ∷ Either ExecCreateError ([ProcIOAction], (ExitVal, Text))
mockTrue = lazily <$> (S.toList ∘ mockProcIO $ mkProc trueC)

mockTrue1 ∷ Either ExecCreateError ([ProcIOAction], (ExitVal, Text))
mockTrue1 = lazily <$> (S.toList ∘ mockProcIO $ mkProc' mock1T trueC)

mockFalse ∷ Either ExecCreateError ([ProcIOAction], (ExitVal, ()))
mockFalse = lazily <$> (S.toList ∘ mockProcIO $ mkProc falseC)

mockFalse1 ∷ Either ExecCreateError ([ProcIOAction], (ExitVal, (Text,Text)))
mockFalse1 = lazily <$> (S.toList ∘ mockProcIO $ mkProc' mock1TT falseC)

runTrue ∷ IO (Either ExecCreateError ([ProcIOAction], (ExitVal, Text)))
runTrue =
  splitMError $ lazily <$> (S.toList ∘ runProcIO' defOpts $ mkProc trueC)

runTrue_ ∷ IO (Either ExecCreateError ([ProcIOAction], Text))
runTrue_ =
  splitMError $ lazily <$> (S.toList ∘ runProcIO' defOpts $ mkProc_ trueC)

-- run true, but with mock set to exit 1; shouldn't have an effect
runTrue1 ∷ IO (Either ExecCreateError ([ProcIOAction], (ExitVal, Text)))
runTrue1 =
  splitMError $
      lazily <$> (S.toList $ (runProcIO' defOpts $ mkProc' mock1T trueC))

-- run true, but with mock set to exit 1 and the exit value "checked";
-- shouldn't have an effect
runTrue1_ ∷ IO (Either ExecCreateError ([ProcIOAction], Text))
runTrue1_ =
  splitMError $
      lazily <$> (S.toList $ (runProcIO' defOpts $ mkProc'_ mock1T trueC))

runFalse ∷ IO (Either ExecCreateError ([ProcIOAction], (ExitVal, Text)))
runFalse =
  splitMError $
      lazily <$> (S.toList $ (runProcIO' defOpts $ mkProc falseC))

runFalse_ ∷ IO (Either ExecCreateError ([ProcIOAction], Text))
runFalse_ =
  splitMError $
      lazily <$> (S.toList ∘ runProcIO' defOpts $ mkProc_ falseC)

-- run false, but with mock set to exit 1; shouldn't have an effect
runFalse1 ∷ IO (Either ExecCreateError ([ProcIOAction], (ExitVal, (Text,Text))))
runFalse1 =
  splitMError $
      lazily <$> (S.toList $ (runProcIO' defOpts $ mkProc' mock1TT falseC))

-- run false, but with mock set to exit 1 and the exit value "checked";
-- shouldn't have an effect
runFalse1_ ∷ IO (Either ExecCreateError ([ProcIOAction], (Text,Text)))
runFalse1_ =
  splitMError $
      lazily <$> (S.toList $ (runProcIO' defOpts $ mkProc'_ mock1TT falseC))

echoC ∷ Text → CmdSpec
echoC t = CmdSpec Paths.echo [ "-n", t ]


echoFoo ∷ CmdSpec
echoFoo = echoC "foo"

catC ∷ Text → CmdSpec
catC t = CmdSpec Paths.cat [ t ]

echoCat ∷ (MonadError ε η, AsCreateProcError ε, AsExecError ε) ⇒
           Text → ProcIO ε η (Text,Text)
echoCat t = mkProc_ (echoC t) >>= mkProc'_ mock0TT ∘ catC

echoCat_ ∷ (MonadError ε η, AsCreateProcError ε, AsExecError ε) ⇒
            Text → ProcIO ε η ([Text],[Text])
echoCat_ t = mkProc_ (echoC t) >>= mkProc_ ∘ catC
-}

{-
-- to prove it works with Text as well as (Text,Text)
echoCat' ∷ (MonadError ε η, AsCreateProcError ε, AsExecError ε) ⇒
            Text → ProcIO ε η Text
echoCat' t = mkProc_ (echoC t) >>= mkProc_ ∘ catC
-}

{-
echoCatM ∷ (MonadError ε η, AsCreateProcError ε, AsExecError ε) ⇒
            Natural → Text → ProcIO ε η Text
echoCatM m t = do
  x ← mkProc_ (echoC t)
  (y,()) ← mkProc'_ (defCPOpts & mockLvl .~ m) $ catC x
  return y


runEchoCat ∷ Text → IO (Either ExecCreateError ([ProcIOAction], (Text,Text)))
runEchoCat t =
  splitMError ∘ fmap lazily ∘ S.toList ∘ runProcIO' defOpts $ echoCat t


runEchoCat_ ∷ Text
            → IO (Either ExecCreateError ([ProcIOAction], ([Text],[Text])))
runEchoCat_ t =
  splitMError ∘ fmap lazily ∘ S.toList ∘ runProcIO' defOpts $ echoCat_ t

runEchoCat' ∷ Text → IO (Either ExecCreateError ([ProcIOAction], Text))
runEchoCat' t =
  splitMError ∘ fmap lazily ∘ S.toList ∘ runProcIO' defOpts $ echoCat' t


mockEchoCat ∷ Text → Either ExecCreateError ([ProcIOAction], (Text,Text))
mockEchoCat t = fmap lazily ∘ S.toList ∘ mockProcIO $ echoCat t


doEchoCat ∷ (HasVerboseLevel m θ, HasDryRunLevel n θ) ⇒
             θ → IO (Either ExecCreateError (Text,Text))
doEchoCat opts = splitMError $ doProcIO opts (echoCat "/dev/null")

doEchoCat' ∷ (HasVerboseLevel m θ, HasDryRunLevel n θ) ⇒
             θ → IO (Either ExecCreateError Text)
doEchoCat' opts = splitMError $ doProcIO opts (echoCatM 2 "/dev/null")
-}

{-
noMockOpt  ∷ MyOption
noMockOpt = MyOption dryRunOff (VerboseLevel Nil)

mockOpt  ∷ MyOption
mockOpt   = MyOption dryRunOn  (VerboseLevel Nil)
-}
trueFalseTests ∷ TestTree
trueFalseTests =
  testGroup "trueFalseTests"
    [ {- testCase "mock true" $
        assertRight (assertEqual "mock true" ([DoMockCmd trueC],(ExitVal 0,"")))
                    mockTrue

    , testCase "true mock fail" $
        assertRight (assertEqual "mock fail" ([DoMockCmd trueC],(ExitVal 1,"alpha")))
                    mockTrue1

    -- without capture, just to prove it compiles
    , testCase "mock false" $
        assertRight (assertEqual "mock false" ([DoMockCmd falseC],(ExitVal 0, ())))
                    mockFalse

    , testCase "false mock fail" $
        assertRight (assertEqual "mock fail"
                                 ([DoMockCmd falseC],(ExitVal 1, ("beta","gamma"))))
                    mockFalse1

    , testCase "run true" $
        runTrue >>=
          assertRight (assertEqual "run true" ([DoCmd trueC],(ExitVal 0,"")))

    , testCase "run true (check)" $
        runTrue_ >>=
          assertRight (assertEqual "run true" ([DoCmd trueC],""))

    , testCase "run true (mock fail)" $
        runTrue1 >>=
          assertRight (assertEqual "run true" ([DoCmd trueC],(ExitVal 0,"")))

    , testCase "run true (mock fail + check)" $
        runTrue1_ >>=
          assertRight (assertEqual "run true" ([DoCmd trueC],""))

    , testCase "run false" $
        runFalse >>=
          assertRight (assertEqual "run false" ([DoCmd falseC],(ExitVal 1,"")))

    , testCase "run false (check)" $
        runFalse_ >>=
          assertLeft (assertEqual "run false" (ecExecE' falseC (ExitVal 1) mockT))

    -- with capture stderr, just to prove it compiles
    , testCase "run false (mock fail)" $
        runFalse1 >>=
          assertRight (assertEqual "run false" ([DoCmd falseC],(ExitVal 1,("",""))))

    , testCase "run false (mock fail + check)" $
        runFalse1_ >>=
          assertLeft (assertEqual "run false" (ecExecE' falseC (ExitVal 1) mockTT))
-}
    ]

echoTests ∷ TestTree
echoTests =
  testGroup "echo"
    [ {- testCase "mock" $
        assertRight (assertEqual "no mock output" ([DoMockCmd echoFoo],(ExitVal 0,"")))
                    mockEchoFoo

    , testCase "mock" $
        assertRight (assertEqual "no mock output"
                                 ([DoMockCmd echoFoo],(ExitVal 1,"alpha")))
                    mockEchoFoo1

    , testCase "mock fail" $
        assertLeft (assertEqual "mock fail output"
                                (ecExecE' echoFoo (ExitVal 1) ("alpha" ∷ Text)))
                   mockEchoFooFail

    , testCase "real" $
        runEchoFoo >>= assertRight (assertEqual "echo output"
                                                ([DoCmd echoFoo],(ExitVal 0,"foo")))
-}
    ]

echoCatTests ∷ TestTree
echoCatTests =
  testGroup "echo-cat"
    [
{-
      testCase "mock" $
        assertRight (assertEqual "no mock output"
                                  ([ -- DoMockCmd $ echoC "/etc/group"
                                   -- , DoMockCmd $ catC ""
                                   ],
                                   ("delta","epsilon")))
                    (mockEchoCat "/etc/group")

      testCase "real" $
        runEchoCat "/etc/group" >>=
          assertRight ( \ (cmds,(t,e)) → do
                            let ts = lines t
                            case ts of
                              []    → assertFailure "no lines"
                              (x:_) → assertBool ("root first: " <> unpack x)
                                                  (x =="root:x:0:")
                            assertEqual "stderr" "" e
                            assertEqual "echo cmd"
                                        [ -- DoCmd $ echoC "/etc/group"
                                        -- , DoCmd $ catC "/etc/group"
                                        ] cmds
                      )

    , testCase "real [Text]" $
        runEchoCat_ "/etc/group" >>=
          assertRight ( \ (cmds,(ts,es)) → do
                            case ts of
                              []    → assertFailure "no lines"
                              (x:_) → assertBool ("root first: " <> unpack x)
                                                  (x =="root:x:0:")
                            assertEqual "stderr" [] es
                            assertEqual "echo cmd"
                                        [ -- DoCmd $ echoC "/etc/group"
                                        -- , DoCmd $ catC "/etc/group"
                                        ] cmds
                      )

    , testCase "real Text" $
        runEchoCat' "/etc/group" >>=
          assertRight ( \ (cmds,t) → do
                            let ts = lines t
                            case ts of
                              []    → assertFailure "no lines"
                              (x:_) → assertBool ("root first: " <> unpack x)
                                                  (x =="root:x:0:")
                            assertEqual "echo cmd"
                                        [ -- DoCmd $ echoC "/etc/group"
                                        -- , DoCmd $ catC "/etc/group"
                                        ] cmds
                      )

    , testCase "fail" $
        runEchoCat "/etc/puorg" >>=
          assertLeft (\x → case x ^? _ExecError of
                              Nothing → assertFailure "no ExecError"
                              Just e'  → do
--                                assertEqual "cmdspec" (catC "/etc/puorg")
--                                                      (e' ^. cmdspec)
                                assertEqual "exitval" (ExitVal 1)
                                                      (e' ^. exitval)
                                assertEqual "stdout"  (Just "")
                                                      (e' ^. stdout)
                                case e' ^. stderr of
                                  Nothing → assertFailure "no stderr"
                                  Just e  → assertBool ("stderr: " <> unpack e)
                                                        ("No such file"
                                                                  `isInfixOf` e)
                     )

    , testCase "doProcIO mock" $
        doEchoCat mockOpt >>= assertEqual "" (Right ("delta","epsilon"))
    , testCase "doProcIO real" $
        doEchoCat noMockOpt >>= assertEqual "" (Right ("",""))
    , testCase "doProcIO' real (high mock value)" $
        doEchoCat' mockOpt >>=
          assertLeft (\x → case x ^? _ExecError of
                         Nothing → assertFailure "farmer"
                         Just e  → do
--                           assertEqual "cmdspec" (catC "") (e ^. cmdspec)
                           assertEqual "exitval" (ExitVal 1) (e ^. exitval)
                           assertEqual "stdout"  (Just "") (e ^. stdout)
                           case e ^. stderr of
                             Nothing → assertBool "no stderr" True
                             Just s  → assertFailure ("stderr: " <> unpack s)
                     )
-}
    ]

------------------------------------------------------------

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = do
  _ ← runTestsP_ tests p
  return ()

tests ∷ TestTree
tests = testGroup "ProcLib.Process" [ trueFalseTests, echoTests, echoCatTests
                                    , systemTests, captureTests
                                    , monadTests
                                    ]

{-
_testExec ∷ IO (Either CreateProcError Exited)
_testExec = do
  let go ∷ Natural → IO (Either CreateProcError Exited)
      go m = splitMError ∘ runProcIO (defOpts & verboseL .~ 1 & dryRunL .~ m) $
                mkExec (CmdSpec Paths.grep [ "root", "/etc/passwd" ])
  -- when used, will cause an ExitFailure 253 (because we mocked an exec)
  -- _ ← go 1
  go 0
-}

-- that's all, folks! ---------------------------------------------------------
