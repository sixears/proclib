{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ProcLib.Types.MakeProc
  ( CreateProc, MakeProc( makeProc ), cmd_spec, cp_opts, std_err, std_in, std_out )
where

-- base --------------------------------

import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Bool               ( Bool( False, True ) )
import Data.Eq                 ( (==) )
import Data.Function           ( (.), ($) )
import Data.Functor            ( (<$>) )
import Data.Maybe              ( Maybe( Just, Nothing ), maybe )
import System.IO               ( Handle )

-- env-plus ----------------------------

import Env.Types  ( strsEnv )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.Review  ( (#) )
import Control.Lens.TH      ( makeLenses )

-- monaderror-io -----------------------

import MonadError     ( mapMError, splitMError )
import MonadError.IO  ( asIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⫥) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- path --------------------------------

-- import Path  ( toFilePath )

-- process -----------------------------

import qualified  System.Process  as  SysProc

import System.Process  ( CreateProcess( CreateProcess )
                       , ProcessHandle
                       , StdStream( CreatePipe, Inherit, NoStream )
                       , createProcess_
                       )

-- text --------------------------------

import Data.Text  ( unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcErr ) )
import ProcLib.Types.CmdSpec          ( CmdSpec, HasCmdSpec( cmdSpec )
                                      , argsStrs, cmdStr, exeFP )
import ProcLib.Types.CreateProcOpts   ( CreateGroup( CreateGroup )
                                      , CreateProcOpts
                                      , HasCreateProcOpts( createProcOpts )
                                      , cmdName, createGroup, cwd, env
                                      )
import ProcLib.Types.MkStream         ( MkStream, mkStream )

--------------------------------------------------------------------------------

-- CreateProc ----------------------------------------------

data CreateProc ω = CreateProc { _cmd_spec :: CmdSpec
                               , _std_in   :: StdStream
                               , _std_out  :: StdStream
                               , _std_err  :: StdStream
                               , _cp_opts  :: CreateProcOpts ω
                               }


$( makeLenses ''CreateProc )

instance HasCreateProcOpts (CreateProc ω) ω where
  createProcOpts = cp_opts

instance HasCmdSpec (CreateProc ω) where
  cmdSpec = cmd_spec

-- MakeProc ------------------------------------------------

createProc_ :: (MonadIO μ, AsCreateProcError ε, MonadError ε μ) =>
               CreateProc ω
            -> μ (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProc_ cp = do
  p <- splitMError . asIOError $
           createProcess_ (maybe (cp ^. cmdStr) unpack (cp ^. cmdName)) $
               CreateProcess { SysProc.cmdspec =
                                   SysProc.RawCommand (cp ^. exeFP) (cp ^. argsStrs)
                             , SysProc.cwd     = (⫥ filepath) ⊳ (cp ^. cwd)

                             , SysProc.env     = strsEnv <$> cp ^. env
                             , SysProc.std_in  = cp ^. std_in
                             , SysProc.std_out = cp ^. std_out
                             , SysProc.std_err = cp ^. std_err

                             , SysProc.create_group  =
                                   cp ^. createGroup == CreateGroup

                             , SysProc.close_fds     = True
                             , SysProc.delegate_ctlc = True
                             , SysProc.new_session   = False
                             , SysProc.child_group   = Nothing
                             , SysProc.child_user    = Nothing

                             , SysProc.detach_console     = False -- windoze only
                             , SysProc.create_new_console = False -- windoze only
                             , SysProc.use_process_jobs   = False -- windoze only
                             }

  let p' = mapMError (_CreateProcErr #) p
  join (return p')

class MakeProc ω where
  makeProc :: (MonadIO μ, AsCreateProcError ε, MonadError ε μ) =>
              CreateProcOpts δ -> MkStream -> CmdSpec -> μ (ProcessHandle, ω)

instance MakeProc () where
  makeProc opts stdIn c = do
    inH <- mkStream stdIn
    -- lazy (irrefutable) pattern: we "know" this will always work; if it
    -- doesn't, something fundamentally is broken within CreateProc and
    -- recovery is surely not TRT.
    ~(Nothing, Nothing, Nothing, h) <-
      createProc_ CreateProc { _cmd_spec = c
                             , _std_in   = inH
                             , _std_out  = Inherit
                             , _std_err  = Inherit
                             , _cp_opts  = opts
                             }
    return (h, ())

instance MakeProc Handle where
  makeProc opts stdIn c = do
    inH <- mkStream stdIn
    ~(Nothing, Just outH, Nothing, h) <- createProc_
                                            CreateProc { _cmd_spec = c
                                                       , _std_in   = inH
                                                       , _std_out  = CreatePipe
                                                       , _std_err  = Inherit
                                                       , _cp_opts  = opts
                                                       }
    return (h, outH)

instance MakeProc (Handle,()) where
  makeProc opts stdIn c = do
    inH <- mkStream stdIn
    ~(Nothing, Just outH, Nothing, h) <- createProc_
                                          CreateProc { _cmd_spec = c
                                                     , _std_in   = inH
                                                     , _std_out  = CreatePipe
                                                     , _std_err  = NoStream
                                                     , _cp_opts  = opts
                                                     }
    return (h, (outH,()))

instance MakeProc ((),Handle) where
  makeProc opts stdIn c = do
    inH <- mkStream stdIn
    ~(Nothing, Nothing, Just errH, h) <- createProc_
                                            CreateProc { _cmd_spec = c
                                                       , _std_in   = inH
                                                       , _std_out  = NoStream
                                                       , _std_err  = CreatePipe
                                                       , _cp_opts  = opts
                                                       }
    return (h, ((),errH))

instance MakeProc ((),()) where
  makeProc opts stdIn c = do
    inH <- mkStream stdIn
    ~(Nothing, Nothing, Nothing, h) <- createProc_
                                            CreateProc { _cmd_spec = c
                                                       , _std_in   = inH
                                                       , _std_out  = NoStream
                                                       , _std_err  = NoStream
                                                       , _cp_opts  = opts
                                                       }
    return (h, ((),()))

instance MakeProc (Handle,Handle) where
  makeProc opts stdIn c = do
    inH <- mkStream stdIn
    ~(Nothing, Just outH, Just errH, h) <- createProc_
                                            CreateProc { _cmd_spec = c
                                                       , _std_in   = inH
                                                       , _std_out  = CreatePipe
                                                       , _std_err  = CreatePipe
                                                       , _cp_opts  = opts
                                                       }
    return (h, (outH,errH))


-- that's all, folks! ----------------------------------------------------------
