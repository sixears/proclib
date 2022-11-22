{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ProcLib.T.Ping
  ( tests )
where

-- base --------------------------------

import Data.Either    ( Either )
import Data.Function  ( ($), (&) )
import System.IO      ( IO )

-- fluffy ------------------------------

import Fluffy.MonadError  ( splitMError )
import Fluffy.MonadIO     ( MonadIO, say )

-- lens --------------------------------

import Control.Lens.Setter  ( (?~), (.~) )

-- mtl ---------------------------------

import Control.Monad.Trans  ( lift )

-- path --------------------------------

import Path  ( mkAbsFile )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Paths     as  Paths
import qualified  ProcLib.Ping.Opt  as  Ping

import ProcLib.Ping               ( PingError, ping', pingCmd, pingTxt )
import ProcLib.Ping.Opt           ( PingOpt, mkOptsPing )
import ProcLib.Ping.Req           ( pingCount, pingDeadline, pingReqDefault )
import ProcLib.Process            ( runProcIO )
import ProcLib.Types.CmdSpec      ( CmdSpec( CmdSpec ) )
import ProcLib.Types.RunProcOpts  ( defOpts, verboseL )

-------------------------------------------------------------------------------

sayT :: MonadIO m => Text -> m ()
sayT = say

_testIO :: IO (Either PingError ())
_testIO = splitMError $ runProcIO (defOpts & verboseL .~ 1) $ do
  lift $ sayT "this should work...\n"
  ping' "localhost" $(mkOptsPing [Ping.count 3])
  lift $ sayT "\n...and this should not...\n"
  ping' "nosuchhost" $(mkOptsPing [Ping.count 3])

----------------------------------------

_test :: IO ()
_test = defaultMain tests

tests :: TestTree
tests = testGroup "ProcLib.Ping" [ pingTxtTest, pingCmdTest ]

pingTxtTest :: TestTree
pingTxtTest =
  testGroup "pingTxtTest"
    [ testCase "pingTxt" $
              pingTxt "myhost" (pingReqDefault & pingCount ?~ 55
                                               & pingDeadline ?~ 33)
          @?= CmdSpec Paths.ping ["myhost", "-c", "55", "-w", "33"]
    ]

pingCmdTest :: TestTree
pingCmdTest =
  let bazQuux = $( mkAbsFile "/baz/quux" )
   in testGroup "pingCmdTest"
        [ testCase "pingCmd" $     pingCmd "myhost" ([] :: [PingOpt])
                               @?= CmdSpec Paths.ping ["myhost"]

        , testCase "pingCmd (options)" $
                  pingCmd "myhost"
                           ([ Ping.path bazQuux, Ping.count 3
                            , Ping.deadline 5] :: [PingOpt])

              @?= CmdSpec bazQuux ["myhost", "-c", "3", "-w", "5"]
       ]

-- that's all, folks! ---------------------------------------------------------
