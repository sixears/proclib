{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}

module ProcLib.CommonOpt.Verbose
  ( Verbose, VerboseLevel( VerboseLevel )
  , HasVerboseLevel( verboseLevel )

  , flagVerbose, verboseP, verbose2P
  , ifVerbose, ifVerboseGE

  , unlessVerboseGE, verboseLvl, verboseOn, verboseOff
  )
where

import Prelude ( )

-- base --------------------------------

import Data.Bool        ( Bool, bool )
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

-- | re-order (kinda 'rotate' version of 'flip') args to bool to make currying
--   easier
ifThenElse :: Bool -> a -> a -> a
ifThenElse b t e = bool e t b

-- Verbose -------------------------------------------------

data VerboseN = VerboseN
  deriving Show

newtype VerboseLevel n = VerboseLevel (AtMost n VerboseN)

-- makeClassy works fine - but causes haddock to hang :-(
-- $( makeClassy ''VerboseLevel )
class HasVerboseLevel n c | c -> n where
  verboseLevel :: Lens' c (VerboseLevel n)
  level        :: Lens' c (AtMost n VerboseN)
  {-# INLINE level #-}
  level       =  verboseLevel . level

instance HasVerboseLevel n (VerboseLevel n) where
  verboseLevel = id
  level        = iso ( \ (VerboseLevel x) -> x) VerboseLevel

instance HasVerboseLevel n (AtMost n VerboseN) where
  verboseLevel = lens VerboseLevel (const . view level)
              {- lens VerboseLevel (const getAtMost)
                 where getAtMost (VerboseLevel x) = x -}

instance Show (VerboseLevel n) where
  show (VerboseLevel v) = [fmt|Verbose: %d|] (count v)

type Verbose = VerboseLevel One

verboseLvl :: HasVerboseLevel n s => s -> Natural
verboseLvl = verboseLvl' . view verboseLevel

verboseLvl' :: VerboseLevel n -> Natural
verboseLvl' (VerboseLevel v) = count v

ifVerboseGE :: HasVerboseLevel n h => Natural -> h -> a -> a -> a
ifVerboseGE i a = ifThenElse (verboseLvl a >= i)

ifVerbose :: HasVerboseLevel n h => h -> a -> a -> a
ifVerbose = ifVerboseGE 1

unlessVerboseGE :: HasVerboseLevel n h => Natural -> h -> a -> a -> a
unlessVerboseGE i a d n = ifVerboseGE i a n d

----------------------------------------

flagVerbose :: Parser VerboseN
flagVerbose =
  flag' VerboseN
        (long "verbose" <> short 'v' <> help "increase output verbosity")

------------------------------------------------------------

verboseOff :: VerboseLevel ('S n)
verboseOff = VerboseLevel Nil

verboseOn :: VerboseLevel ('S n)
verboseOn  = VerboseLevel (Cons VerboseN Nil)

----------------------------------------

verboseP :: Parser Verbose
verboseP = VerboseLevel <$> atMostOne flagVerbose

----------

verbose2P :: Parser (VerboseLevel Two)
verbose2P = VerboseLevel <$> atMostTwo flagVerbose

-- that's all, folks! ---------------------------------------------------------
