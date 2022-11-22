{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | a class of things that collect many (including none) file Handles, and
--   a way to slurp them all in to provide the Text for each

module ProcLib.Types.Handles
  ( Handles( slurp ) )
where

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($) )
import System.IO               ( Handle )

-- text --------------------------------

import Data.Text     ( Text, lines )
import Data.Text.IO  ( hGetContents )

--------------------------------------------------------------------------------

class Handles ζ ths | ths -> ζ where
  slurp :: MonadIO μ => ζ -> μ ths

instance Handles () () where
  -- | no handles, no text slurped
  slurp () = return ()

instance Handles Handle Text where
  -- | slurped Text for stdout (but not stderr, which is untouched)
  slurp (h0) = do
    t0 <- liftIO $ hGetContents h0
    return (t0)

instance Handles Handle [Text] where
  -- | slurped Text for stdout (but not stderr, which is untouched)
  slurp (h0) = do
    t0 <- liftIO $ hGetContents h0
    return $ lines t0

instance Handles (Handle, Handle) (Text, Text) where
  -- | slurped Text for each of stdout,stderr
  slurp (h0, h1) = do
    t0 <- liftIO $ hGetContents h0
    t1 <- liftIO $ hGetContents h1
    return (t0,t1)

instance Handles (Handle, ()) (Text, ()) where
  -- | slurped Text for stdout, stderr closed
  slurp (h0,()) = do
    t0 <- liftIO $ hGetContents h0
    return (t0,())

instance Handles ((), Handle) ((), Text) where
  -- | slurped Text for stderr, stdout closed
  slurp ((),h1) = do
    t1 <- liftIO $ hGetContents h1
    return ((),t1)

instance Handles ((), ()) ((), ()) where
  -- | slurped Text for stderr, stdout closed
  slurp ((),()) = return ((),())

instance Handles (Handle, Handle) ([Text], [Text]) where
  -- | slurped Text for each of stdout,stderr
  slurp (h0, h1) = do
    t0 <- liftIO $ hGetContents h0
    t1 <- liftIO $ hGetContents h1
    return $ (lines t0, lines t1)

-- that's all, folks! ----------------------------------------------------------
