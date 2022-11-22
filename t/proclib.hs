{-# LANGUAGE OverloadedStrings #-}

import Prelude ( )

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Types.T.CreateProcOpts  as  CreateProcOpts
import qualified  ProcLib.Types.T.MkStream        as  MkStream

-- import qualified  ProcLib.T.Ping                  as  Ping
import qualified  ProcLib.T.Process               as  Process
-- import qualified  ProcLib.T.Wc                    as  Wc

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "proclib" [ MkStream.tests, CreateProcOpts.tests
                            -- , Ping.tests
                            , Process.tests
                            -- , Wc.tests
                            ]
