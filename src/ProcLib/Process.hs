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
-- # can we reduce the ??/ths in ProcExecCtxt to just one?
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

import Data.Function.Unicode  ( (???) )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.), view )

-- monaderror-io -----------------------

import MonadError.IO  ( ?? )

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

mkExec' ??? (HasCreateProcOpts ?? (), HasMockLvl ??, AsCreateProcError ??) ???
           ?? ??? CmdSpec ??? ProcIO ?? ?? Exited

mkExec' opts cmd = mkCmd $ do
  executor ??? view (directExec (opts ^. mockLvl))
  lift $ executor opts cmd

mkExec ??? AsCreateProcError ?? ??? CmdSpec ??? ProcIO ?? ?? Exited
mkExec = mkExec' defCPOpts

----------------------------------------

errorIfNonZeroExit  ??? (ExitVal ??? x ??? ??) ??? (ExitVal, x) ??? Maybe ??
errorIfNonZeroExit _ (ExitVal 0, _) = Nothing
errorIfNonZeroExit f (ev,x)         = Just $ f ev x

--------------------------------------------------------------------------------

-- | Create a process ready for running with runProcIO

mkProc' ??? (HasMockLvl ??, HasCreateProcOpts ?? (ExitVal,??),
            AsCreateProcError ??, MockDefault ??, Handles ?? ??, MakeProc ??) ???
           ?? ??? CmdSpec ??? ProcIO ?? ?? (ExitVal, ??)

mkProc' opts cmd = mkCmd $ do
  executor ??? view (childExec (opts ^. mockLvl))
  lift $ executor (opts ^. mock) opts (opts ^. inH) cmd

-- | `mkProc'`, but with default options
mkProc ??? (AsCreateProcError ??, MockDefault ??, Handles ?? ??, MakeProc ??) ???
          CmdSpec ??? ProcIO ?? ?? (ExitVal, ??)

mkProc = mkProc' defCPOpts

{- | `mkProc'`, but throws an ExecError in case of non-zero (non-signal)
     termination
-}
mkProc'_ ??? (HasCreateProcOpts ?? (ExitVal,??), HasMockLvl ??,
             AsExecError ??, AsCreateProcError ??,
             ToMaybeTexts ??, MakeProc ??, Handles ?? ??, MockDefault ??) ???
            ?? ??? CmdSpec ??? ProcIO ?? ?? ??
mkProc'_ opts cmd = mkProc' opts cmd >>= mkCmd ??? lift ??? execErrorIfNonZero cmd

-- | `mkProc'_`, but with default options
mkProc_ ??? (AsExecError ??, AsCreateProcError ??, ToMaybeTexts ??,
            MakeProc ??, Handles ?? ??, MockDefault ??) ???
           CmdSpec ??? ProcIO ?? ?? ??
mkProc_ = mkProc'_ defCPOpts

----------------------------------------

mkIO' ??? (HasMockLvl ??, MonadIO ??) ??? ?? ??? ?? ??? Text ??? ?? ?? ??? ProcIO ?? ?? ??

mkIO' opts m rpt io = mkPIO $ do
  executor ??? view (ioExec (opts ^. mockLvl))
  lift $ executor m rpt io

mkIO ??? (MockDefault ??, MonadIO ??) ??? Text ??? ?? ?? ??? ProcIO ?? ?? ??
mkIO = mkIO' defMockLvl mockDef

----------------------------------------

-- | central dispatch for running ProcIO
runProcIO__ ??? MonadError ?? ?? ???
               (ProcIOAction ??? ?? ??) ??? ProcExecCtxt ?? ??? ProcIO ?? ?? ??
             ??? Stream (Of ??) ?? ??
runProcIO__ f c = mapM f ??? flip runReaderT c ??? unCmd

runProcIO_ ??? (MonadIO ??, MonadError ?? ??) ???
             ProcExecCtxt ?? ??? Natural ??? ProcIO ?? ?? ??
           ??? Stream (Of ProcIOAction) ?? ??
runProcIO_ c v = runProcIO__ (warnActionR v) c

-- | Run a process stream, with given verbosity and mock level.  No stream of
--   ProcIOAction is returned; this is for when you emit such info to stderr
--   and do nothing further with it.
runProcIO ??? (HasRunProcOpts ??, MonadIO ??, MonadError ?? ??) ???
             ?? ??? ProcIO ?? ?? ?? ??? ?? ??
runProcIO o = S.effects ??? runProcIO' o

----------------------------------------

-- | Run a process stream, with given verbosity and mock level, returning a
--   stream of ProcIOActions.
runProcIO' ??? (HasRunProcOpts ??, MonadIO ??, MonadError ?? ??) ???
              ?? ??? ProcIO ?? ?? ?? ??? Stream (Of ProcIOAction) ?? ??
runProcIO' o = runProcIO_ (choiceCtxt $ o ^. dryRunL) (o ^. verboseL)

----------------------------------------

-- | Fully mock a process stream, with no verbosity, and return a stream of
--   ProcIOActions.
mockProcIO ??? MonadError ?? ?? ??? ProcIO ?? ?? ?? ??? Stream (Of ProcIOAction) ?? ??
mockProcIO = runProcIO__ return mockCtxt

----------------------------------------

-- | Run a process stream, with verbosity & mock level as part of an options
--   object, with HasDryRunLevel & HasVerboseLevel, returning a stream of
--   ProcIOActions.

doProcIO' ??? (HasDryRunLevel n ??, HasVerboseLevel m ??,
             MonadIO ??, MonadError ?? ??) ???
            ?? ??? ProcIO ?? ?? ?? ??? Stream (Of ProcIOAction) ?? ??
doProcIO' o = runProcIO' (RunProcOpts (dryRunLvl o) (verboseLvl o))

----------------------------------------

-- | Run a process stream, with verbosity & mock level as part of an options
--   object, with HasDryRunLevel & HasVerboseLevel.
doProcIO ??? (HasDryRunLevel n ??, HasVerboseLevel m ??,
            MonadIO ??, MonadError ?? ??) ???
           ?? ??? ProcIO ?? ?? ?? ??? ?? ??
doProcIO o = S.effects ??? doProcIO' o

----------------------------------------

-- | Run a process stream, with given verbosity but no mocks, returning a
--   stream of ProcIOActions.
execProcIO' ??? (MonadIO ??, MonadError ?? ??) ???
              Natural ??? ProcIO ?? ?? ?? ??? Stream (Of ProcIOAction) ?? ??
execProcIO' = runProcIO_ execCtxt

----------------------------------------

-- | Run a process stream, with given verbosity but no mocks.
execProcIO ??? (MonadIO ??, MonadError ?? ??) ??? Natural ??? ProcIO ?? ?? ?? ??? ?? ??
execProcIO v = S.effects ??? execProcIO' v

----------------------------------------


{- | Execute a sequence of external commands in IO (subject to mock), with
     chosen verbosity -}
system ??? (HasDryRunLevel ?? ??, HasVerboseLevel ?? ??, Exception ??, MonadIO ??) ???
       ?? ??? ProcIO' ?? (ExceptT ?? IO) ?? ??? ?? ??
system o = ?? ??? doProcIO o

-- that's all, folks! ---------------------------------------------------------
