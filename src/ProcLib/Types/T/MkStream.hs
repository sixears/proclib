{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.Types.T.MkStream
  ( tests )
where

import Prelude ( )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import System.IO      ( IO )
import Text.Show      ( show )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Review  ( (#) )

-- process -----------------------------

import System.Process  ( StdStream( NoStream ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP_ )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.MkStream  ( _MkS, _MkT )

-------------------------------------------------------------------------------

mkStreamTests ∷ TestTree
mkStreamTests =
  testGroup "MkStream"
    [ testCase "StdStream" $ show (_MkS # NoStream) @?= "MkS NoStream"
    , testCase "Text" $ show (_MkT # "my text") @?= "MkT \"my text\""
    , testCase "StdStream lens StdStream" $
          (_MkS # NoStream) ^? _MkS @?= Just NoStream
    , testCase "StdStream lens Text" $
          (_MkT # "my text") ^? _MkS @?= Nothing
    , testCase "Text lens StdStream" $ (_MkS # NoStream) ^? _MkT @?= Nothing
    , testCase "Text lens Text" $ (_MkT # "my text") ^? _MkT @?= Just "my text"
    ]

------------------------------------------------------------

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests s = do
  _ ← runTestsP_ tests s
  return ()

tests ∷ TestTree
tests = testGroup "ProcLib.Types.MkStream" [ mkStreamTests ]

-- that's all, folks! ---------------------------------------------------------
