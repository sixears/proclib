{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Ping.Req
  ( HasPingReq( pingReq ), PingReq
  , pingPath, pingCount, pingDeadline
  , pingReqDefault
  )
where

import Prelude ( )

-- base --------------------------------

import Data.Maybe       ( Maybe( Nothing ) )
import Numeric.Natural  ( Natural )
import Text.Show        ( Show )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile )

-- lens --------------------------------

import Control.Lens.TH  ( makeClassy )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Paths  as  Paths

-------------------------------------------------------------------------------

data PingReq = PingReq { _pingPath     :: AbsFile
                       , _pingCount    :: Maybe Natural
                       , _pingDeadline :: Maybe Natural
                       }
  deriving Show

$( makeClassy ''PingReq )

pingReqDefault :: PingReq
pingReqDefault = PingReq { _pingPath     = Paths.ping
                         , _pingCount    = Nothing
                         , _pingDeadline = Nothing
                         }

-- that's all, folks! ---------------------------------------------------------
