-- | an error that is found by executing an external command; that is, the command
--   ran to completion and exited (not signalled), but the exit code (or possibly
--   some text, if parsed) indicates an error.

module ProcLib.Error.ExecError
  ( AsExecError(..), ExecError, ToMaybeTexts( toMaybeTexts )
  , asExecError, cmdspec, errorIfNonZero, errorIfNonZero', execErrorIfNonZero
  , execErrorIfNonZero', execErrorNull, execErrorNull', exitval, stdout, stderr
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Either        ( Either( Right ) )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( (.), ($), (&), id )
import Data.Functor       ( (<$>), fmap )
import Data.Maybe         ( Maybe( Just, Nothing ), catMaybes )
import Data.Monoid        ( (<>) )
import Data.Word          ( Word8 )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
import Text.Show          ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callStack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )
import Control.Lens.TH      ( makeLenses )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text     ( Text, concat, lines, null, unlines )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt ---------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.CmdSpec  ( CmdSpec )
import ProcLib.Types.ExitVal  ( ExitVal( ExitVal ) )

--------------------------------------------------------------------------------

{-
class AsExecError χ where
  _ExecError ∷ Prism' χ ExecError

instance AsExecError ExecError where
  _ExecError = id
-}

------------------------------------------------------------

class ToMaybeTexts ω where
  toMaybeTexts ∷ ω → (Maybe Text, Maybe Text)

instance ToMaybeTexts () where
  toMaybeTexts _ = (Nothing, Nothing)

instance ToMaybeTexts Text where
  toMaybeTexts t = (Just t, Nothing)

instance ToMaybeTexts [Text] where
  toMaybeTexts t = (Just $ unlines t, Nothing)

instance ToMaybeTexts (Text,Text) where
  toMaybeTexts (out,err) = (Just out, Just err)

instance ToMaybeTexts (Text,()) where
  toMaybeTexts (out,()) = (Just out, Nothing)

instance ToMaybeTexts ((),()) where
  toMaybeTexts ((),()) = (Nothing, Nothing)

instance ToMaybeTexts ([Text],[Text]) where
  toMaybeTexts (out,err) = (Just $ unlines out, Just $ unlines err)

------------------------------------------------------------

data ExecError = ExecError_ { _cmdspec ∷ CmdSpec
                            , _exitval ∷ ExitVal
                            , _stdout  ∷ Maybe Text
                            , _stderr  ∷ Maybe Text
                            , _cstack  ∷ CallStack
                            }
  deriving Show

$( makeLenses ''ExecError )

--------------------

instance Exception ExecError

--------------------

instance Eq ExecError where
  ExecError_ cs1 ex1 so1 se1 _ == ExecError_ cs2 ex2 so2 se2 _ =
    (cs1,ex1,so1,se1) == (cs2,ex2,so2,se2)

--------------------

instance HasCallstack ExecError where
  callstack = lens _cstack (\ ee cs → ee & cstack ⊢ cs)

--------------------

-- elide redundant pattern-match from makeClassyPrisms
-- $( makeClassyPrisms ''ExecError )
class AsExecError r_aDXa where
  _ExecError ∷ Prism' r_aDXa ExecError
  _ExecError_ ∷
    Prism' r_aDXa (CmdSpec, ExitVal, Maybe Text,
                                     Maybe Text)
  _ExecError_ = ((.) _ExecError) _ExecError_

instance AsExecError ExecError where
  _ExecError = id
  _ExecError_ =
    (prism (\ (c, ev, so, se) → ExecError_ c ev so se callStack))
           ( \ (ExecError_ c ev stdo stde _) → Right (c,ev,stdo,stde)
           )


instance Printable ExecError where
  print (ExecError_ cspec ev out err _) =
    let p t l = if null l
                then t <> ": <EMPTY>\n"
                else unlines (fmap ((t <> "> ") <>) (lines l))
     in P.text . concat . catMaybes $ [ p "STDOUT" <$> out
                                      , p "STDERR" <$> err
                                      , Just $ [fmt|%w: %w|] ev cspec
                                      ]

------------------------------------------------------------

asExecError ∷ ∀ ε ω . (AsExecError ε, ToMaybeTexts ω, HasCallStack) ⇒
              CmdSpec → ExitVal → ω → ε
asExecError c x w = let (o,e) = toMaybeTexts w
                     in _ExecError # ExecError_ c x o e callStack

execErrorNull ∷ HasCallStack ⇒ CmdSpec → ExitVal → ExecError
execErrorNull cspec ev = ExecError_ cspec ev Nothing Nothing callStack

execErrorNull' ∷ CmdSpec → Word8 → ExecError
execErrorNull' cspec = execErrorNull cspec . ExitVal

errorIfNonZero ∷ MonadError ε μ =>
                  (CmdSpec → ExitVal → ω → ε) → CmdSpec → (ExitVal, ω)
               → μ ω
errorIfNonZero _ _       (ExitVal 0, w) = return w
errorIfNonZero e cspec (ev, w)        = throwError $ e cspec ev w

errorIfNonZero' ∷ MonadError ε μ =>
                   (CmdSpec → ExitVal → ε) → CmdSpec → ExitVal → μ ()
errorIfNonZero' _ _     (ExitVal 0) = return ()
errorIfNonZero' e cspec ev          = throwError $ e cspec ev


execErrorIfNonZero ∷ (AsExecError ε, MonadError ε μ, ToMaybeTexts ω) =>
                      CmdSpec → (ExitVal, ω) → μ ω
execErrorIfNonZero = errorIfNonZero asExecError

execErrorIfNonZero' ∷ (MonadError ExecError μ, ToMaybeTexts ω) =>
                      CmdSpec → (ExitVal, ω) → μ ω
execErrorIfNonZero' = execErrorIfNonZero

-- that's all, folks! ----------------------------------------------------------
