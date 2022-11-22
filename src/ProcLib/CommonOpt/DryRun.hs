{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}

module ProcLib.CommonOpt.DryRun
  ( DryRun, DryRunLevel( DryRunLevel ), HasDryRunLevel( dryRunLevel )
  , dryRunLvl, dryRunOff, dryRunOn, dryRunP, dryRun2P
  , flagDryRun, ifDryRun, ifDryRunEq, ifDryRunGE, ifThenElse
  , unlessDryRunGE
  )
where

import Prelude  ( )

-- base --------------------------------

import Data.Bool        ( Bool )
import Data.Eq          ( (==) )
import Data.Function    ( (.), const, id )
import Data.Functor     ( (<$>) )
import Data.Monoid      ( (<>) )
import Data.Ord         ( (>=) )
import Numeric.Natural  ( Natural )
import Text.Show        ( Show( show ) )

-- fmt ---------------------------------

import Text.Fmt  ( fmt )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( iso )
import Control.Lens.Lens    ( Lens', lens )

-- natural -----------------------------

import Natural  ( AtMost( Cons, Nil ), Nat( S ), One, Two
                , atMostOne, atMostTwo, count )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, flag', help, long, short )

-------------------------------------------------------------------------------

-- | curryable if-then-else
ifThenElse :: Bool -> a -> a -> a
ifThenElse b t e = if b then t else e

------------------------------------------------------------

data DryRunN = DryRunN
  deriving Show

newtype DryRunLevel n = DryRunLevel { _level :: AtMost n DryRunN }

-- makeClassy works fine - but causes haddock to hang :-(
-- $( makeClassy ''DryRunLevel )
class HasDryRunLevel n c | c -> n where
  dryRunLevel :: Lens' c (DryRunLevel n)
  level       :: Lens' c (AtMost n DryRunN)
  {-# INLINE level #-}
  level       =  dryRunLevel . level

instance HasDryRunLevel n (DryRunLevel n) where
  dryRunLevel = id
  level       = iso ( \ (DryRunLevel x) -> x) DryRunLevel

instance HasDryRunLevel n (AtMost n DryRunN) where
  dryRunLevel = lens DryRunLevel (const . view level)

instance Show (DryRunLevel n) where
  show (DryRunLevel d) = [fmt|DryRun: %d|] (count d)

type DryRun = DryRunLevel One

dryRunLvl :: HasDryRunLevel n s => s -> Natural
dryRunLvl = dryRunLvl' . view dryRunLevel

dryRunLvl' :: DryRunLevel n -> Natural
dryRunLvl' (DryRunLevel d) = count d

ifDryRunEq :: HasDryRunLevel n h => Natural -> h -> a -> a -> a
ifDryRunEq i a = ifThenElse (dryRunLvl a == i)

ifDryRunGE :: HasDryRunLevel n h => Natural -> h -> a -> a -> a
ifDryRunGE i a = ifThenElse (dryRunLvl a >= i)

ifDryRun :: HasDryRunLevel n h => h -> a -> a -> a
ifDryRun = ifDryRunGE 1

unlessDryRunGE :: HasDryRunLevel n h => Natural -> h -> a -> a -> a
unlessDryRunGE i a d n = ifDryRunGE i a n d

----------------------------------------

flagDryRun :: Parser DryRunN
flagDryRun = flag' DryRunN (long "dry-run" <> short 'n'
                                           <> help "don't really run")

dryRunOff :: DryRunLevel ('S n)
dryRunOff = DryRunLevel Nil

dryRunOn :: DryRunLevel ('S n)
dryRunOn  = DryRunLevel (Cons DryRunN Nil)

----------------------------------------

dryRunP :: Parser DryRun
dryRunP = DryRunLevel <$> atMostOne flagDryRun

----------

dryRun2P :: Parser (DryRunLevel Two)
dryRun2P = DryRunLevel <$> atMostTwo flagDryRun

-- that's all, folks! ---------------------------------------------------------
