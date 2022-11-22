{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.Ping.Opt
  ( AsPingOpt( _PingOpt ) , PingOpt
  , mkOptsPing, pingOpts, path, deadline, count )
where

import Prelude ( id )

-- base --------------------------------

import Data.Bool        ( Bool )
import Data.Eq          ( Eq( (/=) ) )
import Data.Foldable    ( Foldable, foldr )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.Monoid      ( First )
import Numeric.Natural  ( Natural )
import Text.Show        ( Show )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( Getting, (^.) )
import Control.Lens.Review  ( (#) )
import Control.Lens.Setter  ( (.~), (?~) )
import Control.Lens.TH      ( makeClassyPrisms )

-- path --------------------------------

import FPath.AbsFile  ( AbsFile )

-- template-haskell --------------------

import Language.Haskell.TH         ( Q, Exp, appE, conE )
import Language.Haskell.TH.Syntax  ( Lift( lift ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Opt               ( Opt( cmpOpt )
                                 , cmpOptDefaultLensed
                                 , liftAbsFile, mkOpts
                                 )
import ProcLib.Ping.Req           ( PingReq
                                 , pingPath, pingCount, pingDeadline
                                 , pingReqDefault
                                 )

-------------------------------------------------------------------------------

prismMatch ∷ Eq a ⇒ Getting (First a) s a → s → Bool
prismMatch l o = o ^? l /= Nothing

------------------------------------------------------------

data PingOpt = PING_PATH (AbsFile)
             | PING_COUNT Natural | PING_DEADLINE Natural
  deriving (Eq, Show)

$( makeClassyPrisms ''PingOpt )

{-
instance Opt PingOpt where
  cmpOpt = cmpOptDefaultLensed "ping"
                               [ (prismMatch _PING_PATH,     "path")
                               , (prismMatch _PING_COUNT,    "count")
                               , (prismMatch _PING_DEADLINE, "deadline")
                               ]
                               []

-}
{-
instance Lift PingOpt where
  lift (PING_PATH p)     = liftAbsFile 'PING_PATH p
  lift (PING_COUNT u)    = appE (conE 'PING_COUNT) (lift u)
  lift (PING_DEADLINE p) = appE (conE 'PING_DEADLINE) (lift p)
-}

-------------------------------------------------------------------------------

path :: AsPingOpt o => AbsFile -> o
path p = _PING_PATH # p

count :: AsPingOpt o => Natural -> o
count c = _PING_COUNT # c

deadline :: AsPingOpt o => Natural -> o
deadline d = _PING_DEADLINE # d

mkOptsPing :: [PingOpt] -> Q Exp
mkOptsPing = mkOpts

-------------------------------------------------------------------------------

-- | Apply a list of options to the default req, to create a customized
--   request.  Note that earlier options take precedence over later options.
pingOpts :: (AsPingOpt o, Foldable t) => t o -> PingReq
pingOpts = foldr od pingReqDefault
          where optDecode :: PingOpt -> PingReq -> PingReq
                optDecode (PING_PATH p)     = pingPath      .~ p
                optDecode (PING_COUNT c)    = pingCount     ?~ c
                optDecode (PING_DEADLINE d) = pingDeadline  ?~ d

                od :: (AsPingOpt o) => o -> PingReq -> PingReq
                od x = case x ^? _PingOpt of
                         Just x' -> optDecode x'
                         Nothing -> id

-- that's all, folks! ---------------------------------------------------------
