{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TemplateHaskell     #-}

module ProcLib.Types.T.CreateProcOpts
  ( tests )
where

import Prelude ( )

-- base --------------------------------

import Control.Monad   ( return )
-- import Data.Bifunctor  ( first )
import Data.Function   ( {- (.), -} ($), (&) )
-- import Data.Functor    ( (<$>), fmap )
import Data.Maybe      ( Maybe( Just, Nothing ) )
import Data.String     ( String )
import System.IO       ( IO )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

-- import Data.Textual  ( toText )

-- env-plus ----------------------------

import Env.Types  ( {- Env( Env, unEnv ), -} fromList )

-- fpath --------------------------------

import FPath.AbsDir  ( absdir )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.Review  ( (#) )
import Control.Lens.Setter  ( (.~), (?~) )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad  ( (⪼) )

-- process -----------------------------

import System.Process  ( StdStream( CreatePipe, Inherit, NoStream ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP_ )

-- text --------------------------------

import Data.Text  ( Text, empty )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.CreateProcOpts  ( CreateGroup( CreateGroup, NoCreateGroup )
                                     , CreateProcOpts
                                     , cwd, env, cmdName
                                     , createGroup, inH, withInS, withInT, mock
                                     )
import ProcLib.Types.MkStream        ( _MkS, _MkT )

-------------------------------------------------------------------------------

createProcOptsTests :: TestTree
createProcOptsTests =
  let nonsuch = [absdir|/nonsuch/|]
      myenv   = fromList [("foo", "bar")]
      c0 :: CreateProcOpts Text = def
      c1 :: CreateProcOpts (Text,Text) = def & cwd ?~ nonsuch
                                             & env ?~ myenv
                                             & createGroup .~ CreateGroup
                                             & cmdName ?~ "mycmd"
                                             & mock .~ ("alpha","beta")
                                             & inH  .~ _MkS # Inherit
      c2 :: CreateProcOpts () = def `withInS` CreatePipe
      c3 :: CreateProcOpts () = def & inH  .~ _MkT # "tupni"
      c4 :: CreateProcOpts () = def `withInT` "inptu"
      c5 :: CreateProcOpts () = def `withInT` "input"
  in testGroup "CreateProcOpts"
    [ testCase "def cwd"         $ c0 ^. cwd         @?= Nothing
{-
    , testCase "def env"         $
          -- fmaps required because at the time of writing, Env isn't
          -- show/printable
          fmap (first toText) . unEnv <$> (c0 ^. env) @?= Nothing
-}
    , testCase "def createGroup" $ c0 ^. createGroup @?= NoCreateGroup
    , testCase "def cmdName"     $ c0 ^. cmdName     @?= Nothing
    , testCase "def mock"        $ c0 ^. mock        @?= empty
    , testCase "def inH"         $ c0 ^. inH         @?= _MkS # NoStream

--    , testCase "c1 cwd"         $ c1 ^. cwd         @?= Just nonsuch
{-
    , testCase "c1 env"         $
          -- fmaps required because at the time of writing, Env isn't
          -- show/printable
          fmap (first toText) . unEnv <$> (c1 ^. env) @?= Just [("foo","bar")]
-}
    , testCase "c1 createGroup" $ c1 ^. createGroup @?= CreateGroup
    , testCase "c1 cmdName"     $ c1 ^. cmdName     @?= Just "mycmd"
    , testCase "c1 mock"        $ c1 ^. mock        @?= ("alpha","beta")
    , testCase "c1 inH"         $ c1 ^. inH         @?= _MkS # Inherit

    , testCase "c2 cwd"         $ c2 ^. cwd         @?= Nothing
{-
    , testCase "c2 env"         $
          -- fmaps required because at the time of writing, Env isn't
          -- show/printable
          fmap (first toText) . unEnv <$> (c2 ^. env) @?= Nothing
-}
    , testCase "c2 createGroup" $ c2 ^. createGroup @?= NoCreateGroup
    , testCase "c2 cmdName"     $ c2 ^. cmdName     @?= Nothing
    , testCase "c2 mock"        $ c2 ^. mock        @?= ()
    , testCase "c2 inH"         $ c2 ^. inH         @?= _MkS # CreatePipe

    , testCase "c3 cwd"         $ c3 ^. cwd         @?= Nothing
{-
    , testCase "c3 env"         $
          -- fmaps required because at the time of writing, Env isn't
          -- show/printable
          fmap (first toText) . unEnv <$> (c3 ^. env) @?= Nothing
-}
    , testCase "c3 createGroup" $ c3 ^. createGroup @?= NoCreateGroup
    , testCase "c3 cmdName"     $ c3 ^. cmdName     @?= Nothing
    , testCase "c3 mock"        $ c3 ^. mock        @?= ()
    , testCase "c3 inH"         $ c3 ^. inH         @?= _MkT # "tupni"

    , testCase "c4 cwd"         $ c4 ^. cwd         @?= Nothing
{-
    , testCase "c4 env"         $
          -- fmaps required because at the time of writing, Env isn't
          -- show/printable
          fmap (first toText) . unEnv <$> (c4 ^. env) @?= Nothing
-}
    , testCase "c4 createGroup" $ c4 ^. createGroup @?= NoCreateGroup
    , testCase "c4 cmdName"     $ c4 ^. cmdName     @?= Nothing
    , testCase "c4 mock"        $ c4 ^. mock        @?= ()
    , testCase "c4 inH"         $ c4 ^. inH         @?= _MkT # "inptu"

    , testCase "c5 cwd"         $ c5 ^. cwd         @?= Nothing
{-
    , testCase "c5 env"         $
          -- fmaps required because at the time of writing, Env isn't
          -- show/printable
          fmap (first toText) . unEnv <$> (c5 ^. env) @?= Nothing
-}
    , testCase "c5 createGroup" $ c5 ^. createGroup @?= NoCreateGroup
    , testCase "c5 cmdName"     $ c5 ^. cmdName     @?= Nothing
    , testCase "c5 mock"        $ c5 ^. mock        @?= ()
    , testCase "c5 inH"         $ c5 ^. inH         @?= _MkT # "input"
    ]

------------------------------------------------------------

_test :: IO ()
_test = defaultMain tests

_tests :: String -> IO ()
_tests s = runTestsP_ tests s ⪼ return ()

tests :: TestTree
tests = testGroup "ProcLib.Types.CreateProcOpts" [ createProcOptsTests ]

-- that's all, folks! ---------------------------------------------------------
