{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Wc.Opt
  ( AsWcOpt, WcOpt
  , bytes, chars, busybox, lines, maxLineLength, mkOptsWc
  , path, pathAndroid, wcOpts, words )
where

import Prelude ( id )

-- base --------------------------------

import Data.Bool      ( Bool( True ) )
import Data.Eq        ( Eq )
import Data.Foldable  ( Foldable, foldr )
import Data.Function  ( (.) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Text.Show      ( Show )

-- fluffy ------------------------------

import Fluffy.Lens  ( prismMatch )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Review  ( (#) )
import Control.Lens.Setter  ( (.~), set )
import Control.Lens.TH      ( makeClassyPrisms )

-- path --------------------------------

import Path  ( Abs, File, Path, mkRelFile )

-- template-haskell --------------------

import Language.Haskell.TH         ( Q, Exp, appE, conE )
import Language.Haskell.TH.Syntax  ( Lift( lift ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.CommonOpt.Busybox  ( AsBusyboxOpt( _BusyboxOpt )
                                  , BusyboxOpt( ANDROID_BUSYBOX )
                                  , androidBusyboxBin, busybox
                                  )
import ProcLib.Opt                ( Opt( cmpOpt )
                                  , cmpOptDefaultLensed, cmpOptPrism
                                  , cmpOptPrisms
                                  , liftAbsFile, mkOpts
                                  )
import ProcLib.Wc.Req             ( WcReq
                                  , wcBusyboxOpts, wcBytes, wcChars
                                  , wcLines, wcMaxLineLength
                                  , wcPath, wcReqDefault, wcWords
                                  )

-------------------------------------------------------------------------------

data WcPureOpt = WC_PATH (Path Abs File) | WC_BYTES | WC_CHARS | WC_LINES
               | WC_MAX_LINE_LENGTH | WC_WORDS
  deriving (Eq, Show)

$( makeClassyPrisms ''WcPureOpt )

instance Lift WcPureOpt where
  lift WC_BYTES           = conE 'WC_BYTES
  lift WC_CHARS           = conE 'WC_CHARS
  lift WC_LINES           = conE 'WC_LINES
  lift WC_MAX_LINE_LENGTH = conE 'WC_MAX_LINE_LENGTH
  lift WC_WORDS           = conE 'WC_WORDS
  lift (WC_PATH p)        = liftAbsFile 'WC_PATH p

instance Opt WcPureOpt where
  cmpOpt = cmpOptDefaultLensed "wc" [(prismMatch _WC_PATH, "path")] []

-------------------------------------------------------------------------------

androidPath :: Path Abs File
androidPath = androidBusyboxBin $( mkRelFile "wc" )

bytes :: AsWcPureOpt o => o
bytes = _WC_BYTES # ()

chars :: AsWcPureOpt o => o
chars = _WC_CHARS # ()

lines :: AsWcPureOpt o => o
lines = _WC_LINES # ()

maxLineLength :: AsWcPureOpt o => o
maxLineLength = _WC_MAX_LINE_LENGTH # ()

words :: AsWcPureOpt o => o
words = _WC_WORDS # ()

path :: AsWcPureOpt o => Path Abs File -> o
path p = _WC_PATH # p

pathAndroid :: AsWcPureOpt o => o
pathAndroid = _WC_PATH # androidPath

-------------------------------------------------------------------------------

data WcOpt = WC_PURE_OPT WcPureOpt | BUSYBOX_OPT BusyboxOpt
  deriving (Eq, Show)

$( makeClassyPrisms ''WcOpt )

instance Opt WcOpt where  -- delegate opt comparisons to the constituents
  cmpOpt = cmpOptPrisms [ cmpOptPrism  _WC_PURE_OPT  WC_PURE_OPT    
                        , cmpOptPrism  _BUSYBOX_OPT  BUSYBOX_OPT
                        ] 

instance Lift WcOpt where
  lift (WC_PURE_OPT l) = appE (conE 'WC_PURE_OPT) (lift l)
  lift (BUSYBOX_OPT c) = appE (conE 'BUSYBOX_OPT)  (lift c)

instance AsWcPureOpt WcOpt where
  _WcPureOpt = _WC_PURE_OPT

instance AsBusyboxOpt WcOpt where
  _BusyboxOpt = _BUSYBOX_OPT

mkOptsWc :: [WcOpt] -> Q Exp
mkOptsWc = mkOpts

-------------------------------------------------------------------------------

wcOpts :: (AsWcOpt o, AsBusyboxOpt o, Foldable t) => t o -> WcReq
wcOpts = foldr od wcReqDefault
         where optDecode :: WcPureOpt -> WcReq -> WcReq
               optDecode WC_BYTES           = set wcBytes True
               optDecode WC_CHARS           = set wcChars True
               optDecode WC_LINES           = set wcLines True
               optDecode WC_MAX_LINE_LENGTH = set wcMaxLineLength True
               optDecode WC_WORDS           = set wcWords True
               optDecode (WC_PATH p)        = set wcPath p

               busyboxOptDecode :: BusyboxOpt -> WcReq -> WcReq
               busyboxOptDecode ANDROID_BUSYBOX =
                 (wcPath .~ androidPath) . (wcBusyboxOpts .~ True)

               od :: (AsWcOpt o, AsBusyboxOpt o) => o -> WcReq -> WcReq
               od x = case x ^? _WcOpt of
                        Just x' -> case x' of
                                     WC_PURE_OPT l -> optDecode l
                                     BUSYBOX_OPT c -> busyboxOptDecode c
                        Nothing -> case x ^? _BusyboxOpt of
                                     Just b  -> busyboxOptDecode b
                                     Nothing -> id

-- that's all, folks! ---------------------------------------------------------
