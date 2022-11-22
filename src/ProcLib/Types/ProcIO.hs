{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes   #-}

module ProcLib.Types.ProcIO
  ( ProcIO, ProcIO', mkCmd, mkPIO, unCmd )
where

-- base --------------------------------

import Control.Applicative  ( Applicative, (<*>), pure )
import Control.Monad        ( Monad, (>>=) )
import Data.Function        ( (.) )
import Data.Functor         ( Functor, fmap )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Reader  ( ReaderT )
import Control.Monad.Trans   ( MonadTrans, lift )

-- streaming ---------------------------

import Streaming.Prelude  ( Of, Stream )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Types.ProcExecCtxt     ( ProcExecCtxt )
import ProcLib.Types.ProcIOAction     ( ProcIOAction )

--------------------------------------------------------------------------------

type ProcIO ε η ω = MonadError ε η => ProcIO' ε η ω

data ProcIO' ε η ω =
    Cmd { unCmd :: MonadError ε η =>
                   ReaderT (ProcExecCtxt η) (Stream (Of ProcIOAction) η) ω }

instance (Monad η) => Functor (ProcIO' ε η) where
  fmap :: (α -> β) -> ProcIO' ε η α -> ProcIO' ε η β
  fmap f (Cmd a) = Cmd (fmap f a)

instance (Monad η) => Applicative (ProcIO' ε η) where
  pure    = Cmd . pure
  (<*>) :: ProcIO' ε η (α -> β) -> ProcIO' ε η α -> ProcIO' ε η β
  Cmd f <*> Cmd xs = Cmd (f <*> xs)


instance Monad η => Monad (ProcIO' ε η) where
  Cmd c  >>= f = Cmd (c  >>= unCmd . f)

instance MonadTrans (ProcIO' ε) where
  lift :: Monad η => η α -> ProcIO' ε η α
  lift = Cmd . lift . lift

mkCmd :: AsCreateProcError ε =>
         ReaderT (ProcExecCtxt η) (Stream (Of ProcIOAction) η) ω -> ProcIO' ε η ω
mkCmd = Cmd

mkPIO :: MonadError ε η =>
         ReaderT (ProcExecCtxt η) (Stream (Of ProcIOAction) η) ω -> ProcIO' ε η ω
mkPIO = Cmd

-- that's all, folks! ----------------------------------------------------------
