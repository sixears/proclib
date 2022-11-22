{-# LANGUAGE TupleSections #-}

module ProcLib.Optparse.Complete
  ( completeStr, completeStrStr, completeStrStrStr
  , completer', completer, completer_ )
where

import Prelude ( )

-- base --------------------------------

import Data.Function        ( (.), ($), id )
import Data.Functor         ( (<$>), fmap )
import Data.Monoid          ( (<>), mconcat )
import Data.String          ( String )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- optparse-applicative ----------------

import Options.Applicative  ( ArgumentFields, Mod, Parser, command
                            , completeWith, idm , info, strArgument, subparser)

-------------------------------------------------------------------------------

-- | create a string Parser that completes with a list of strings
completeStr :: Mod ArgumentFields String -> [String] -> Parser String
completeStr argFields comps =
  strArgument (argFields <> completeWith comps)

-- | create a string Parser that takes two arguments; the set of completions
--   are given as a tree of depth 2; the first level is the set of completions
--   of the first argument, the second level is the set of completions for each
--   of the initial completions
completeStrStr :: [(String, [String])] -> Parser (String, String)
completeStrStr = completer (completeStr idm)

-- | like `completeStrStr`, but more so
completeStrStrStr :: [(String, [(String, [String])])]
                  -> Parser (String, (String, String))
completeStrStrStr = completer completeStrStr


-- | completion builder, by taking a list of strings, each with their own
--   subsequent possible values, and turning each into a 'command'.  Note that
--   the leading fn is used to turn the possible value into a parser.
completer  :: (t1 -> Parser t) -> [(String, t1)] -> Parser (String, t)
completer = completer' id

completer_  :: Printable v => (t1 -> Parser t) -> [(v, t1)] -> Parser (v, t)
completer_ = completer' toString

completer'  :: (v -> String) -> (t1 -> Parser t) -> [(v, t1)] -> Parser (v, t)
completer' g f xs = subparser . mconcat $ fmap mkCmd xs
              where -- make a Command by prefixing a value parser with a
                    -- string; being sure to return both in the parsed value
                    mkCmd (c,ss) = command (g c) . (`info` idm) $ (c,) <$> f ss

