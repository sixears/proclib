{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Ping
  ( AsPingError( _PingError ), PingError, ping, ping', pingCmd, pingTxt )
where

import Prelude ( )

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Foldable      ( Foldable, concat )
import Data.Function      ( ($), id )
import Data.Maybe         ( Maybe( Just ) )
import Text.Show          ( Show )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( (^.) )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )
import Control.Lens.TH      ( makePrisms )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Trans   ( lift )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError )
import ProcLib.Error.ExecError        ( errorIfNonZero' )
import ProcLib.Opt                    ( maybeOptShow )
import ProcLib.Ping.Opt               ( AsPingOpt, PingOpt, pingOpts )
import ProcLib.Ping.Req               ( HasPingReq
                                      , pingCount, pingDeadline, pingPath )
import ProcLib.Process                ( ExitVal, mkProc )
import ProcLib.Types                  ( HostName )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.ProcIO           ( ProcIO )

-------------------------------------------------------------------------------

class AsPingError ε where
  _PingError :: Prism' ε PingError

data PingError = PingExitE CmdSpec ExitVal | PingCreateE CreateProcError
  deriving (Eq, Show)

$(makePrisms ''PingError)

instance Exception PingError

instance AsPingError PingError where
  _PingError = prism' id Just

instance AsCreateProcError PingError where
  _CreateProcError = prism' PingCreateE (^? _PingCreateE)

mkPingError :: AsPingError ε => CmdSpec -> ExitVal ->  ε
mkPingError cmdspec ev = _PingError # PingExitE cmdspec ev

-------------------------------------------------------------------------------

pingTxt :: HasPingReq s => HostName -> s -> CmdSpec
pingTxt h req =
  CmdSpec (req ^. pingPath)
          (toText h : concat [ maybeOptShow req pingCount    "-c"
                             , maybeOptShow req pingDeadline "-w"
                             ])

----------------------------------------

pingCmd :: (Foldable t, AsPingOpt o) =>
          HostName -> t o -> CmdSpec
pingCmd h opts = pingTxt h (pingOpts opts)

----------------------------------------

ping :: (Foldable φ, AsPingOpt o,
         AsPingError ε, AsCreateProcError ε, MonadError ε η) =>
        HostName -> φ o -> ProcIO ε η ()
ping h opts = do 
  let cmdspec = pingCmd h opts
  (exit,()) <- mkProc cmdspec
  lift $ errorIfNonZero' mkPingError cmdspec exit


----------------------------------------

-- | specialization of ping to use PingOpt to save users having to
--   provide the type
ping' :: (Foldable φ, AsPingError ε, AsCreateProcError ε, MonadError ε η) =>
         HostName -> φ PingOpt -> ProcIO ε η ()
ping' =  ping

-- that's all, folks! ---------------------------------------------------------
