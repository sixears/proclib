{-# LANGUAGE FlexibleContexts #-}

module ProcLib.Optparse.Parsers
  ( readAbsDir, readAbsDir', readAbsFile )
where

import Prelude ()

-- base --------------------------------

import Control.Monad  ( (>>=), return )
import Data.Either    ( either )
import Data.Function  ( (.) )
import Data.String    ( String )
import Text.Show      ( show )

-- fpath ------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( FPathError )
import FPath.Parseable         ( parse )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( ReadM, readerError, str )

-------------------------------------------------------------------------------

readAbsDir :: ReadM AbsDir
readAbsDir = let parseDir :: MonadError FPathError μ => String -> μ AbsDir
                 parseDir = parse
              in str >>= either (readerError . show) return . parseDir

readAbsDir' :: String -> ReadM AbsDir
readAbsDir' = either (readerError . show) return . parse @_ @FPathError

----------------------------------------

readAbsFile :: ReadM AbsFile
readAbsFile = let parseFile :: MonadError FPathError μ => String -> μ AbsFile
                  parseFile = parse
               in str >>= either (readerError . show) return . parseFile

-- that's all, folks! ---------------------------------------------------------
