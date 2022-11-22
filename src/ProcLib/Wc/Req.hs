{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Wc.Req
  ( HasWcReq, WcReq(..), wcBusyboxOpts, wcBytes, wcChars, wcLines
  , wcMaxLineLength, wcPath, wcReqDefault, wcWords )
where

import Prelude ( )

-- base --------------------------------

import Data.Bool  ( Bool( False ) )
import Data.Eq    ( Eq )
import Text.Show  ( Show )

-- lens --------------------------------

import Control.Lens.TH      ( makeClassy )

-- path --------------------------------

import Path  ( Abs, File, Path )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Paths  as  Paths

-------------------------------------------------------------------------------

data WcReq = WcReq { _wcPath          :: Path Abs File
                   , _wcBusyboxOpts   :: Bool
                   , _wcBytes         :: Bool
                   , _wcChars         :: Bool
                   , _wcLines         :: Bool
                   , _wcMaxLineLength :: Bool
                   , _wcWords         :: Bool
                   }
  deriving (Eq, Show)

$( makeClassy ''WcReq )

wcReqDefault :: WcReq
wcReqDefault  = WcReq { _wcPath          = Paths.wc
                      , _wcBusyboxOpts   = False
                      , _wcBytes         = False
                      , _wcChars         = False
                      , _wcLines         = False
                      , _wcMaxLineLength = False
                      , _wcWords         = False
                      }


-- that's all, folks! ---------------------------------------------------------
