{-# LANGUAGE TemplateHaskell #-}

module ProcLib.Types.MkStream
  ( MkStream, _MkT, _MkS, mkStream )
where

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Eq                 ( Eq )
-- import Data.Function           ( (.) )
import Data.Functor            ( (<$>) )
import Data.Monoid             ( (<>) )
import Text.Show               ( Show( show ) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( FPathIOError )

-- lens --------------------------------

import Control.Lens.TH  ( makePrisms )

-- monaderror-io -----------------------

import MonadError.IO  ( ӝ )

-- monadio-plus ------------------------

import MonadIO.Temp  ( tempfile )

-- process -----------------------------

import System.Process  ( StdStream( UseHandle ) )

-- text --------------------------------

import Data.Text  ( Text )

--------------------------------------------------------------------------------

-- | a stream primed with some Text, e.g., to use as a input to a process
textStream :: MonadIO μ => Text -> μ StdStream
textStream t = UseHandle <$> ӝ (tempfile @FPathIOError t)

data MkStream = MkS StdStream | MkT Text
  deriving Eq

instance Show MkStream where
  show (MkS s) = "MkS " <> show s
  show (MkT t) = "MkT " <> show t


mkStream :: MonadIO μ => MkStream -> μ StdStream
mkStream (MkS s) = return s
mkStream (MkT t) = textStream t

$( makePrisms ''MkStream )

-- that's all, folks! ----------------------------------------------------------
