{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UnicodeSyntax          #-}

module ProcLib.Types.ProcExecCtxt
  ( Exited, ProcExecCtxt( ProcExecCtxt )
  , childExec, choiceCtxt, directExec, execCtxt, exited, ioExec, mockCtxt )
where

-- base --------------------------------

import Control.Monad     ( Monad, join, return, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool         ( Bool( False ), otherwise )
import Data.Either       ( Either( Left, Right ) )
import Data.Function     ( ($) )
import Data.Maybe        ( Maybe( Just, Nothing ) )
import Data.Ord          ( (>), (>=) )
import Numeric.Natural   ( Natural )
import Prelude           ( (-), fromIntegral )
import System.Exit       ( ExitCode( ExitFailure, ExitSuccess ) )
import System.IO.Unsafe  ( unsafePerformIO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- env-plus ----------------------------

import Env.Types  ( strsEnv )

-- exited ------------------------------

import Exited  ( Exited, exited, exitWith )

-- lens --------------------------------

import Control.Lens.Getter  ( Getter, to )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError         ( mapMError', splitMError )

-- monadio-plus ------------------------

import MonadIO.Directory  ( chdir )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (≫), (⪼) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )
import Control.Monad.Trans   ( lift )

-- process -----------------------------

import System.Process  ( ProcessHandle, waitForProcess )

-- streaming ---------------------------

import Streaming.Prelude  ( Of, Stream, yield )

-- text --------------------------------

import Data.Text  ( Text )

-- unix --------------------------------

import System.Posix.Process  ( createProcessGroupFor, executeFile
                             , getProcessGroupID )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcErr ) )
import ProcLib.Types.CreateProcOpts   ( CreateGroup( CreateGroup )
                                      , HasCreateProcOpts
                                      , createGroup, createProcOpts, cwd, env )
import ProcLib.Types.CmdSpec          ( CmdSpec, argsStrs, exeFP )
import ProcLib.Types.ExitVal          ( ExitVal( ExitSig, ExitVal ) )
import ProcLib.Types.Handles          ( Handles( slurp ) )
import ProcLib.Types.MakeProc         ( MakeProc( makeProc ) )
import ProcLib.Types.MkStream         ( MkStream )
import ProcLib.Types.ProcIOAction     ( ProcIOAction( DoCmd, DoMockCmd
                                                    , DoExec, DoMockExec
                                                    , DoIO, DoMockIO
                                                    )
                                      )
import ProcLib.Types.Signal           ( Signal( Signal ) )

--------------------------------------------------------------------------------

-- | An execution context; that is, an encapsulation of a way to take a ProcIO
--   computation, and evaluating it - e.g., by executing external processes, or
--   'mocking' them.
--
--  μ is the execution monad - e.g., MonadIO for 'real' executions, Monad for
--  mocks.
data ProcExecCtxt μ
   = ProcExecCtxt { _childExec ∷ forall ω ζ ε φ .
                                 (HasCreateProcOpts φ (ExitVal, ω),
                                  MakeProc ζ, Handles ζ ω,
                                  AsCreateProcError ε, MonadError ε μ) ⇒
                                 (ExitVal, ω)
                               → φ
                               → MkStream
                               → CmdSpec
                               → Stream (Of ProcIOAction) μ (ExitVal, ω)
                  , _childExecMock ∷ forall ω ζ ε φ .
                                      (HasCreateProcOpts φ (ExitVal, ω),
                                       MakeProc ζ, Handles ζ ω,
                                       AsCreateProcError ε, MonadError ε μ) ⇒
                                      (ExitVal, ω)
                                   → φ
                                   → MkStream
                                   → CmdSpec
                                   → Stream (Of ProcIOAction) μ (ExitVal, ω)
                  , _ioExec ∷ forall ω ε .
                              (MonadIO μ, MonadError ε μ) ⇒
                              ω
                            → Text
                            → μ ω
                            → Stream (Of ProcIOAction) μ ω
                  , _ioExecMock ∷ forall ω ε . MonadError ε μ ⇒ ω
                                → Text
                                → μ ω
                                → Stream (Of ProcIOAction) μ ω
                  , _directExec ∷ forall ε φ .
                                  (HasCreateProcOpts φ (),
                                   AsCreateProcError ε, MonadError ε μ) ⇒
                                  φ
                                → CmdSpec
                                → Stream (Of ProcIOAction) μ Exited
                  , _directExecMock ∷ forall ε φ .
                                      (HasCreateProcOpts φ (),
                                       AsCreateProcError ε, MonadError ε μ) ⇒
                                      φ
                                    → CmdSpec
                                    → Stream (Of ProcIOAction) μ Exited
                  , _mockExecutionLvl ∷ Natural
                  }

-- makeLenses/makeClassy don't cope with more than 1 quantified var
-- $( makeClassy ''ProcExecCtxt )


childExec ∷ (MakeProc ζ, Handles ζ ω, AsCreateProcError ε, MonadError ε μ,
              HasCreateProcOpts φ (ExitVal, ω)) ⇒
             Natural
          → Getter (ProcExecCtxt μ)
                    ((ExitVal,ω) → φ
                                 → MkStream
                                 → CmdSpec
                                 → Stream (Of ProcIOAction) μ (ExitVal, ω)
                    )
childExec m = to (\ctxt → let l = _mockExecutionLvl ctxt
                            in if l >= m
                               then _childExecMock ctxt
                               else _childExec ctxt
                 )


ioExec ∷ (MonadIO μ, MonadError ε μ) ⇒
          Natural
       → Getter (ProcExecCtxt μ)
                 (ω → Text
                    → μ ω
                    → Stream (Of ProcIOAction) μ ω
                 )

ioExec m = to (\ctxt → let l = _mockExecutionLvl ctxt
                            in if l >= m
                               then _ioExecMock ctxt
                               else _ioExec ctxt
                 )

directExec ∷ (AsCreateProcError ε, MonadError ε μ, HasCreateProcOpts φ ()) ⇒
              Natural
           → Getter (ProcExecCtxt μ)
                     (φ → CmdSpec → Stream (Of ProcIOAction) μ Exited)
directExec m = to (\ctxt → let l = _mockExecutionLvl ctxt
                             in if l >= m
                                then _directExecMock ctxt
                                else _directExec ctxt
                  )

------------------------------------------------------------


exitCode ∷ ExitCode → ExitVal
exitCode ExitSuccess     = ExitVal 0
exitCode (ExitFailure i) | i > 0     = ExitVal $ fromIntegral i
                         | otherwise = ExitSig ∘ Signal ∘ fromIntegral $ 256-i

----------------------------------------

-- | take an opened process and its output handles,
--   wait for the process, slurp the handles, and return the exit val
--   and any output (as Text)

procWait ∷ (MonadIO μ, Handles ζ ths) ⇒
            μ (ProcessHandle, ζ) → μ (ExitVal, ths)
procWait prox = do
  (handle, hs) ← prox
  ex ← liftIO $ waitForProcess handle
  texts ← slurp hs
  return (exitCode ex, texts)

------------------------------------------------------------

childProc ∷ (MakeProc ζ, Handles ζ ω, HasCreateProcOpts φ (ExitVal, ω),
              MonadIO μ, AsCreateProcError ε, MonadError ε μ) ⇒
             (ExitVal, ω) → φ → MkStream → CmdSpec
          → Stream (Of ProcIOAction) μ (ExitVal, ω)

childProc _mock opts inh cspec = do
  yield (DoCmd cspec)
  x ← splitMError $ makeProc (opts ⊣ createProcOpts) inh cspec
  lift $ splitMError x ≫ \case
    Left  e → join ∘ return $ throwError e
    Right r → procWait (return r)

--------------------

mockChildProc ∷ Monad η ⇒
                 ω → z0 → z1 → CmdSpec → Stream (Of ProcIOAction) η ω
mockChildProc m _ _ cspec = yield (DoMockCmd cspec)⪼ return m

------------------------------------------------------------

ioProc ∷ (MonadIO μ, MonadError ε μ) ⇒
          ω → Text → μ ω → Stream (Of ProcIOAction) μ ω

ioProc _mock rpt io = yield (DoIO rpt) ⪼ lift io

mockIoProc ∷ MonadError ε η ⇒ ω → Text → μ ω → Stream (Of ProcIOAction) η ω
mockIoProc m rpt _ = yield (DoMockIO rpt) ⪼ return m

----------------------------------------

setpgrp ∷ MonadIO μ ⇒ μ ()
setpgrp = liftIO $ getProcessGroupID ≫ createProcessGroupFor⪼ return ()

execProc ∷ (MakeProc ζ, Handles ζ (), HasCreateProcOpts φ (),
             AsCreateProcError ε, MonadError ε μ, MonadIO μ) ⇒
            φ → CmdSpec → Stream (Of ProcIOAction) μ Exited

execProc opts cspec = do
  yield (DoExec cspec)

  case opts ⊣ cwd of
    Nothing → return ()
    Just d  → mapMError' (_CreateProcErr #) $ chdir d

  when (opts ⊣ createGroup ≡ CreateGroup) setpgrp
  _ ← liftIO $ executeFile (cspec ⊣ exeFP)         -- full path executable
                            False                    -- do not use PATH
                            (cspec ⊣ argsStrs)      -- cmd arguments
                            (strsEnv ⊳ opts ⊣ env) -- environment
  return exited

----------------------------------------

__unsafeExitWith__ ∷ ExitCode → Exited
__unsafeExitWith__ = unsafePerformIO ∘ exitWith

-- | mock a direct execution
mockExecProc ∷ Monad η ⇒ ζ → CmdSpec → Stream (Of ProcIOAction) η Exited
mockExecProc _ cspec = do
  yield (DoMockExec cspec)
  return $ __unsafeExitWith__ (ExitFailure 253)

-- | within this context, actions are mocked or not at choice (normally defined
--   by the mock level of the action)
choiceCtxt ∷ MonadIO μ ⇒ Natural → ProcExecCtxt μ
choiceCtxt m = ProcExecCtxt childProc mockChildProc ioProc mockIoProc
                            execProc mockExecProc m
--------------------

-- | within this context, there are no mocks; everything is run for real
execCtxt ∷ MonadIO μ ⇒ ProcExecCtxt μ
execCtxt = ProcExecCtxt childProc childProc ioProc ioProc execProc execProc 0

--------------------

-- | within this context, everything is mocked
mockCtxt ∷ Monad μ ⇒ ProcExecCtxt μ
mockCtxt = ProcExecCtxt mockChildProc mockChildProc mockIoProc mockIoProc
                        mockExecProc mockExecProc 0

-- that's all, folks! ----------------------------------------------------------
