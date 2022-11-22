{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | note that system (and capture, et al) will fail (throw an IO exception) if
--   execution (syscall 'exec') fails (e.g., no such executable), as opposed to
--   the executable returning non-zero

module ProcLib.Process
  ( CreateProcOpts, HasCreateProcOpts
  , ExitVal(..), Handles(..), HasExitVal(..)
  , HasStdOutT(..)
  , HasStdErrT(..), ProcExecCtxt, StdStream(..)
  , exitOkay
  , mkExec, mkExec', mkIO, mkIO', mkProc, mkProc', mkProc_, mkProc'_
  , doProcIO, doProcIO', execProcIO, execProcIO'
  , mockProcIO, runProcIO, runProcIO', system

  , errorIfNonZeroExit, throwNotOkay

  )
where

{-
  * how to merge HasDryRunLevel, etc., with runProcOpts?
  * how to provide a default if we did?
  * prefer mock to dry-run?
  * re-export things so users don't have to re-import loads of other
    (dependency) modules, e.g., data-default, lens, etc. (reduce import list of
    envwd)
  * version of runProcIO (etc.?) with pre-split monad (so no need to
    splitMError)?
-}

-- # type def for MonadError ProcIO ?  Use it in envwh
-- # ProcLib class which is just the common re-exports; remove these from
--     Process
-- # Document all the classes!
-- # exec test executable, demonstrating all the options
-- # handles for exec - barf on most (non-Inherit) options, but leave open to
--     (e.g.,)
--   feed from temp, Inherit or close later.
-- # mock level as part of the ProcID
-- # generic IO as part of CmdY
-- # clean up Ping, Wc, etc.; whole lib.  And release?

-- # add myMain that requires an exitCode; have exec produce that
-- # make runProcess_, etc., take HasProcExecCtxt rather than ProcExecCtxt
-- # factor out distinct modules, documenting as we go
-- # factor out parseFile, parseRelFile{,'}, headNull, from Du.hs
-- # add (</>, mk{Abs,Rel}{File,Path}, {Abs,Rel}{File,Dir}, toFileName with
--     Hashable to Fluffy.Path exports
-- # move firstM from Du to Fluffy.Tuple
-- # move absFileToDir from Du to Fluffy.Path
-- # have path compress out "." and ".." entries in
--     {mk,parse}{Rel,Abs}{File,Dir}
-- # move assert* from Du to Fluffy.Tasty
-- # move absDirToFile from ProcLib.T.Du to Fluffy.Path; tighten logic and/or
--     error reporting
-- # move ContainerKeys, ContainerZeroOrMore from Du to Fluffy.Containers
-- # document Process
-- # create a single encompassing newtype for the mechanics
-- # add zero, one, two to Fluffy using Nil, Cons.  Possible none as synonym
--     for zero.  Add verboseNone, verboseZero, verboseOne, verboseTwo;
--     export VerboseN & verboseIncr :: VerboseLevel -> VerboseLevel
-- # factor ContraGetter, assert*, sayT, putLn, into Fluffy;
-- # replace Display with ToText from TextConv (use fmt %T)
-- # remove Busybox port/user opts, that's what .ssh/config is for
-- # use CmdOut everywhere, or nowhere
-- # can we reduce the ζ/ths in ProcExecCtxt to just one?
-- # generalize (that is, unify) system/capture by returns
--     (none,stdout,out+err)?
-- # go back to TypeFamilies?
-- # unify Handles/MakeProc?
-- # use MakeClassy to give HasProcCtxt(3)?
-- # rename showCmd as logCmd
-- # use asks rather than o <- ask; o ^. showCmd
-- # use a fully effectful monad for external commands

import Prelude ( )

-- base --------------------------------

import Control.Exception       ( Exception )
import Control.Monad           ( (>>=), return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($), flip )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Numeric.Natural         ( Natural )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.), view )

-- monaderror-io -----------------------

import MonadError.IO  ( ӝ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, ExceptT )
import Control.Monad.Reader  ( runReaderT )
import Control.Monad.Trans   ( lift )

-- process -----------------------------

import System.Process  ( StdStream( CreatePipe, Inherit, NoStream, UseHandle ) )

-- streaming ---------------------------

import qualified  Streaming.Prelude  as  S

import Streaming.Prelude  ( Of, Stream, mapM )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.CommonOpt.DryRun       ( HasDryRunLevel, dryRunLvl )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel, verboseLvl )
import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError, ToMaybeTexts
                                      , execErrorIfNonZero )
import ProcLib.Types.CmdSpec          ( CmdSpec )
import ProcLib.Types.CreateProcOpts   ( CreateProcOpts, HasCreateProcOpts
                                      , HasMockLvl
                                      , defCPOpts, defMockLvl, inH, mock
                                      , mockLvl
                                      )
import ProcLib.Types.ExitVal          ( ExitVal( ExitSig, ExitVal )
                                      , HasExitVal( exitVal )
                                      , HasStdErrT( stderrT )
                                      , HasStdOutT( stdoutT )
                                      , exitOkay, throwNotOkay
                                      )
import ProcLib.Types.Handles          ( Handles( slurp ) )
import ProcLib.Types.MakeProc         ( MakeProc )
import ProcLib.Types.MockDefault      ( MockDefault( mockDef ) )
import ProcLib.Types.ProcExecCtxt     ( Exited, ProcExecCtxt
                                      , childExec, choiceCtxt, directExec
                                      , execCtxt, ioExec, mockCtxt
                                      )
import ProcLib.Types.ProcIO           ( ProcIO, ProcIO', mkCmd, mkPIO, unCmd )
import ProcLib.Types.ProcIOAction     ( ProcIOAction )
import ProcLib.Types.RunProcOpts      ( HasRunProcOpts
                                      , RunProcOpts( RunProcOpts )
                                      , dryRunL, verboseL
                                      )
import ProcLib.Types.Verbosity        ( warnActionR )

--------------------------------------------------------------------------------

mkExec' ∷ (HasCreateProcOpts φ (), HasMockLvl φ, AsCreateProcError ε) ⇒
           φ → CmdSpec → ProcIO ε η Exited

mkExec' opts cmd = mkCmd $ do
  executor ← view (directExec (opts ^. mockLvl))
  lift $ executor opts cmd

mkExec ∷ AsCreateProcError ε ⇒ CmdSpec → ProcIO ε η Exited
mkExec = mkExec' defCPOpts

----------------------------------------

errorIfNonZeroExit  ∷ (ExitVal → x → ε) → (ExitVal, x) → Maybe ε
errorIfNonZeroExit _ (ExitVal 0, _) = Nothing
errorIfNonZeroExit f (ev,x)         = Just $ f ev x

--------------------------------------------------------------------------------

-- | Create a process ready for running with runProcIO

mkProc' ∷ (HasMockLvl φ, HasCreateProcOpts φ (ExitVal,ω),
            AsCreateProcError ε, MockDefault ω, Handles ζ ω, MakeProc ζ) ⇒
           φ → CmdSpec → ProcIO ε η (ExitVal, ω)

mkProc' opts cmd = mkCmd $ do
  executor ← view (childExec (opts ^. mockLvl))
  lift $ executor (opts ^. mock) opts (opts ^. inH) cmd

-- | `mkProc'`, but with default options
mkProc ∷ (AsCreateProcError ε, MockDefault ω, Handles ζ ω, MakeProc ζ) ⇒
          CmdSpec → ProcIO ε η (ExitVal, ω)

mkProc = mkProc' defCPOpts

{- | `mkProc'`, but throws an ExecError in case of non-zero (non-signal)
     termination
-}
mkProc'_ ∷ (HasCreateProcOpts φ (ExitVal,ω), HasMockLvl φ,
             AsExecError ε, AsCreateProcError ε,
             ToMaybeTexts ω, MakeProc ζ, Handles ζ ω, MockDefault ω) ⇒
            φ → CmdSpec → ProcIO ε η ω
mkProc'_ opts cmd = mkProc' opts cmd >>= mkCmd ∘ lift ∘ execErrorIfNonZero cmd

-- | `mkProc'_`, but with default options
mkProc_ ∷ (AsExecError ε, AsCreateProcError ε, ToMaybeTexts ω,
            MakeProc ζ, Handles ζ ω, MockDefault ω) ⇒
           CmdSpec → ProcIO ε η ω
mkProc_ = mkProc'_ defCPOpts

----------------------------------------

mkIO' ∷ (HasMockLvl φ, MonadIO μ) ⇒ φ → ω → Text → μ ω → ProcIO ε μ ω

mkIO' opts m rpt io = mkPIO $ do
  executor ← view (ioExec (opts ^. mockLvl))
  lift $ executor m rpt io

mkIO ∷ (MockDefault ω, MonadIO μ) ⇒ Text → μ ω → ProcIO ε μ ω
mkIO = mkIO' defMockLvl mockDef

----------------------------------------

-- | central dispatch for running ProcIO
runProcIO__ ∷ MonadError ε η ⇒
               (ProcIOAction → η β) → ProcExecCtxt η → ProcIO ε η α
             → Stream (Of β) η α
runProcIO__ f c = mapM f ∘ flip runReaderT c ∘ unCmd

runProcIO_ ∷ (MonadIO η, MonadError ε η) ⇒
             ProcExecCtxt η → Natural → ProcIO ε η α
           → Stream (Of ProcIOAction) η α
runProcIO_ c v = runProcIO__ (warnActionR v) c

-- | Run a process stream, with given verbosity and mock level.  No stream of
--   ProcIOAction is returned; this is for when you emit such info to stderr
--   and do nothing further with it.
runProcIO ∷ (HasRunProcOpts ρ, MonadIO μ, MonadError ε μ) ⇒
             ρ → ProcIO ε μ α → μ α
runProcIO o = S.effects ∘ runProcIO' o

----------------------------------------

-- | Run a process stream, with given verbosity and mock level, returning a
--   stream of ProcIOActions.
runProcIO' ∷ (HasRunProcOpts ρ, MonadIO μ, MonadError ε μ) ⇒
              ρ → ProcIO ε μ α → Stream (Of ProcIOAction) μ α
runProcIO' o = runProcIO_ (choiceCtxt $ o ^. dryRunL) (o ^. verboseL)

----------------------------------------

-- | Fully mock a process stream, with no verbosity, and return a stream of
--   ProcIOActions.
mockProcIO ∷ MonadError ε η ⇒ ProcIO ε η α → Stream (Of ProcIOAction) η α
mockProcIO = runProcIO__ return mockCtxt

----------------------------------------

-- | Run a process stream, with verbosity & mock level as part of an options
--   object, with HasDryRunLevel & HasVerboseLevel, returning a stream of
--   ProcIOActions.

doProcIO' ∷ (HasDryRunLevel n θ, HasVerboseLevel m θ,
             MonadIO μ, MonadError ε μ) ⇒
            θ → ProcIO ε μ α → Stream (Of ProcIOAction) μ α
doProcIO' o = runProcIO' (RunProcOpts (dryRunLvl o) (verboseLvl o))

----------------------------------------

-- | Run a process stream, with verbosity & mock level as part of an options
--   object, with HasDryRunLevel & HasVerboseLevel.
doProcIO ∷ (HasDryRunLevel n θ, HasVerboseLevel m θ,
            MonadIO μ, MonadError ε μ) ⇒
           θ → ProcIO ε μ α → μ α
doProcIO o = S.effects ∘ doProcIO' o

----------------------------------------

-- | Run a process stream, with given verbosity but no mocks, returning a
--   stream of ProcIOActions.
execProcIO' ∷ (MonadIO μ, MonadError ε μ) ⇒
              Natural → ProcIO ε μ α → Stream (Of ProcIOAction) μ α
execProcIO' = runProcIO_ execCtxt

----------------------------------------

-- | Run a process stream, with given verbosity but no mocks.
execProcIO ∷ (MonadIO μ, MonadError ε μ) ⇒ Natural → ProcIO ε μ α → μ α
execProcIO v = S.effects ∘ execProcIO' v

----------------------------------------


{- | Execute a sequence of external commands in IO (subject to mock), with
     chosen verbosity -}
system ∷ (HasDryRunLevel ν θ, HasVerboseLevel υ θ, Exception ε, MonadIO μ) ⇒
       θ → ProcIO' ε (ExceptT ε IO) α → μ α
system o = ӝ ∘ doProcIO o

-- that's all, folks! ---------------------------------------------------------
