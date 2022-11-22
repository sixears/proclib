{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.Types.CmdSpec
  ( CmdArgs, CmdExe, CmdExeArgs, CmdSpec(..), HasCmdSpec(..)
  , argsStrs, cmdStr, cmdText, cmdTexts, exeArgs, exeFP )
where

-- import Prelude  ( undefined )

-- base --------------------------------

-- import Data.Bifunctor  ( first )
import Data.Eq         ( Eq )
import Data.Function   ( (.), ($) )
import Data.Functor    ( fmap )
import Data.String     ( String )
-- import Data.Tuple      ( uncurry )
import System.IO       ( FilePath )
import Text.Show       ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.AbsFile     ( AbsFile )

-- lens --------------------------------

import Control.Lens.Getter  ( Getter, (^.), to, view )
-- import Control.Lens.Lens    ( lens )
import Control.Lens.TH      ( makeClassy )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( {- (⊣), -} (⫥) )

-- path --------------------------------

-- import Path  ( toFilePath )

-- process -----------------------------

import System.Process  ( showCommandForUser )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

type CmdExe     = AbsFile
type CmdArgs    = [Text]
type CmdExeArgs = (CmdExe, CmdArgs)

data CmdSpec = CmdSpec { _cmdExe :: CmdExe, _cmdArgs :: CmdArgs }
  deriving Eq

$( makeClassy ''CmdSpec )

instance Show CmdSpec where
  show c = showCommandForUser (c ^. exeFP) (c ^. argsStrs)

instance Printable CmdSpec where
  print c = P.text $ pack (show c)

exeFP :: HasCmdSpec h => Getter h FilePath
exeFP = to ((⫥ filepath) . _cmdExe . view cmdSpec)

exeArgs :: HasCmdSpec h => Getter h CmdExeArgs
exeArgs = to $ \ c -> (c ^. cmdExe, c ^. cmdArgs)

argsStrs :: HasCmdSpec h => Getter h [String]
argsStrs = cmdArgs . to (fmap unpack)

cmdTexts :: HasCmdSpec h => Getter h CmdArgs
-- cmdTexts = exeArgs . to (first (pack . toFilePath)) . to (uncurry (:))
cmdTexts = to (_cmdArgs . view cmdSpec)

cmdText :: HasCmdSpec h => Getter h Text
cmdText = cmdStr . to pack

cmdStr :: HasCmdSpec h => Getter h String
cmdStr = to (show . view cmdSpec)

-- that's all, folks! ----------------------------------------------------------
