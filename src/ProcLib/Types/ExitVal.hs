{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module ProcLib.Types.ExitVal
  ( ExitVal(..), HasExitVal(..), HasStdErrT(..), HasStdOutT(..)
  , evOK, evAbnormal, evHelp, evExecFail, evMock
  , exitOkay, throwNotOkay
  )
where

-- base --------------------------------

import Control.Monad  ( when )
import Data.Bool      ( Bool( False, True ), not )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.Word      ( Word8 )
import Text.Show      ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fmt ---------------------------------

import Text.Fmt  ( fmt )

-- lens --------------------------------

import Control.Lens        ( Lens', view )
import Control.Lens.Tuple  ( _1 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.Signal  ( Signal )

-------------------------------------------------------------------------------

{- | process exit status -}

data ExitVal = ExitVal Word8 | ExitSig Signal
  deriving (Eq, Show)

{- Module ‘Fluffy.MonadIO’ does not export ‘ToExitCode(toExitCode)’ (at time of
   writing)

instance ToExitCode ExitVal where
  toExitCode (ExitVal e) = e
  toExitCode (ExitSig _) = ExitFailure 255
-}

exitOkay :: HasExitVal ev => ev -> Bool
exitOkay (view exitVal -> ExitVal 0) = True
exitOkay _                           = False

evOK       :: ExitVal
evOK       =  ExitVal 0

evAbnormal :: ExitVal
evAbnormal = ExitVal 1

evHelp :: ExitVal
evHelp = ExitVal 2

evExecFail :: ExitVal
evExecFail = ExitVal 254

evMock :: ExitVal
evMock = ExitVal 253

instance Printable ExitVal where
  print (ExitVal ev) = P.text $ [fmt|Execution exit %d|] ev
  print (ExitSig es) = P.text $ [fmt|Execution exit on signal %T|] es

class HasExitVal ev where
  exitVal :: Lens' ev ExitVal

instance HasExitVal ExitVal where
  exitVal = id

instance HasExitVal (ExitVal, a) where
  exitVal = _1

-- | given a datum α which HasExitVal, throw an 'error' (created by f) iff
--   the exit is not okay - that is, if it's non-zero
throwNotOkay :: (HasExitVal α, MonadError ε μ) => (α -> ε) -> α -> μ ()
throwNotOkay f ev = when (not $ exitOkay ev) $ throwError (f ev)

class HasStdOutT s where
  stdoutT :: Lens' s Text

class HasStdErrT s where
  stderrT :: Lens' s Text

-- that's all, folks! ---------------------------------------------------------
