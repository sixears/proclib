{-# LANGUAGE TupleSections #-}

import Prelude ( )

-- base --------------------------------

import Control.Applicative  ( (<*>) )
import Data.Function        ( ($) )
import Data.String          ( String )
import System.IO            ( IO, print )

-- optparse-applicative ----------------

import Options.Applicative  ( execParser, helper, info, progDesc )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Optparse.Complete  ( completeStrStrStr )

-------------------------------------------------------------------------------

main :: IO ()
main = do
  let l3a :: [String]
      l3a = [ "boomer", "apollo" ]
      l3b :: [String]
      l3b = [ "mal", "inara" ]
      l3c :: [String]
      l3c = [ "john", "paul" ]
      l3d :: [String]
      l3d = [ "george", "ringo" ]

      l2a :: String
      l2a = "bar"
      l2b :: String
      l2b = "boom"
      l2c :: String
      l2c = "baz"
      l2d :: String
      l2d = "jung"

      l1ab :: String
      l1ab = "foo"
      l1cd :: String
      l1cd = "quux"

  opts <- execParser $ info (helper <*> completeStrStrStr [ (l1ab, [ (l2a, l3a)
                                                                   , (l2b, l3b)
                                                                   ])
                                                          , (l1cd, [ (l2c, l3c)
                                                                   , (l2d, l3d)
                                                                   ])
                                                          ]
                            )
                            (progDesc "this is my description")

  print opts
