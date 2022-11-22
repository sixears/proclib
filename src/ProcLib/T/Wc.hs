{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UnicodeSyntax       #-}

module ProcLib.T.Wc
  ( tests )
where

import Prelude ( )

-- base --------------------------------

import Control.Monad       ( (>>=), return )
import Data.Bool           ( Bool( False, True ) )
import Data.Either         ( Either( Left, Right ) )
import Data.Function       ( (.), ($), (&) )
import Data.List.NonEmpty  ( NonEmpty( (:|) ), (<|) )
import Data.Maybe          ( Maybe( Just, Nothing ) )
import Data.String         ( String )
import System.IO           ( IO )

-- fluffy ------------------------------

import Fluffy.MonadError  ( splitMError )
import Fluffy.MonadIO     ( print )
import Fluffy.Tasty       ( runTestsP_ )

-- lens --------------------------------

import Control.Lens.Setter  ( (.~) )

-- mtl ---------------------------------

import Control.Monad.Trans  ( lift )

-- mono-traversable --------------------

import Data.NonNull  ( NonNull, ncons )

-- path --------------------------------

import Path  ( Path, Abs, File, mkAbsFile )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- text --------------------------------

import Data.Text  ( Text, unlines )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ProcLib.Paths   as  Paths
import qualified  ProcLib.Wc.Opt  as  Wc
import qualified  ProcLib.Wc.Req  as  Req

import ProcLib.Process            ( runProcIO )
import ProcLib.Types.CmdSpec      ( CmdSpec( CmdSpec ) )
import ProcLib.Types.RunProcOpts  ( defOpts, verboseL )
import ProcLib.Wc                 ( WcError
                                  , WcParseLine( WcParseLine, _wcBytes, _wcChars
                                               , _wcFilename, _wcLines
                                               , _wcMaxLineLength, _wcWords
                                               )
                                  , WcParseError( WcParseErrorT )
                                  , lineCount', lineCount1'
                                  , wc', wcCmd', wcParse
                                  )
import ProcLib.Wc.Opt             ( mkOptsWc )
import ProcLib.Wc.Req             ( WcReq, wcReqDefault )

-------------------------------------------------------------------------------

etcPasswd ∷ Path Abs File
etcPasswd = $( mkAbsFile "/etc/passwd" )

etcGroup ∷ Path Abs File
etcGroup  = $( mkAbsFile "/etc/group" )

etcShells ∷ Path Abs File
etcShells = $( mkAbsFile "/etc/shells" )

xbinWc ∷ Path Abs File
xbinWc = $(mkAbsFile "/system/xbin/wc")

fns0 ∷ NonNull [Path Abs File]
fns0 = etcPasswd `ncons` [etcShells]

fns0' ∷ NonEmpty (Path Abs File)
fns0' = etcPasswd <| etcShells :| []

_testIO ∷ IO (Either WcError ())
_testIO = splitMError $ do
  (wcs,err) ← runProcIO (defOpts & verboseL .~ 1) $ wc' fns0 []
  print (wcs,err)
  runProcIO (defOpts & verboseL .~ 1) $ do
    lineCount'  fns0     >>= lift . print
    lineCount1' etcGroup >>= lift . print

----------------------------------------

sampleReq ∷ WcReq
sampleReq = wcReqDefault { Req._wcBytes         = True
                         , Req._wcChars         = True
                         , Req._wcLines         = True
                         , Req._wcMaxLineLength = True
                         , Req._wcWords         = True
                         }

sampleReq2 ∷ WcReq
sampleReq2 = wcReqDefault { Req._wcBytes         = False
                          , Req._wcChars         = False
                          , Req._wcLines         = True
                          , Req._wcMaxLineLength = False
                          , Req._wcWords         = True
                          }

sampleText ∷ Text
sampleText =
  unlines [ "  44   68 2340 2342   82 /etc/passwd"
          , "   6   10   89   89   33 /etc/shells"
          , "  50   78 2429 2431  115 total"
          ]

sampleText2 ∷ Text
sampleText2 =
  unlines [ "  44   68 /etc/passwd"
          , "   6   10 /etc/shells"
          , "  50   78 total"
          ]


wcCmdTest ∷ TestTree
wcCmdTest =
  testGroup "wcCmd"
    [ testCase "default" $
            wcCmd' fns0' []
        @?= CmdSpec Paths.wc [ "--lines", "--", "/etc/passwd", "/etc/shells" ]
    , testCase "lines" $
            wcCmd' fns0' ($(mkOptsWc [ Wc.lines ]))
        @?= CmdSpec Paths.wc [ "--lines", "--", "/etc/passwd", "/etc/shells" ]
    , testCase "multiple opts" $
            wcCmd' fns0'
                   ($(mkOptsWc [ Wc.lines, Wc.chars ]))
        @?= CmdSpec Paths.wc
                   [ "--chars", "--lines", "--", "/etc/passwd", "/etc/shells" ]
    , testCase "busybox" $
            wcCmd' fns0' $(mkOptsWc [ Wc.lines, Wc.chars, Wc.busybox ])
        @?= CmdSpec xbinWc
                    [ "-l", "-m", "--", "/etc/passwd", "/etc/shells" ]
    ]

----------------------------------------

wcParseTest ∷ TestTree
wcParseTest =
  let wcParse' = wcParse
      l1       = " 77 /noparse"
   in testGroup "wcParse"
        [ testCase "wcParse (succeed)" $
            wcParse' sampleReq sampleText @?=
              Right [ (WcParseLine { _wcFilename = $(mkAbsFile "/etc/passwd")
                                   , _wcLines         = Just 44
                                   , _wcWords         = Just 68
                                   , _wcChars         = Just 2340
                                   , _wcBytes         = Just 2342
                                   , _wcMaxLineLength = Just 82
                                   })
                    , (WcParseLine { _wcFilename = $(mkAbsFile "/etc/shells")
                                   , _wcLines         = Just 6
                                   , _wcWords         = Just 10
                                   , _wcChars         = Just 89
                                   , _wcBytes         = Just 89
                                   , _wcMaxLineLength = Just 33
                                   })
                    ]
        , testCase "wcParse (fail wrong cols)" $
            wcParse' sampleReq l1 @?=
              Left (WcParseErrorT "input does not start with a digit: /noparse"
                                  l1)
        , testCase "wcParse (succeed; 2 columns)" $
            wcParse' sampleReq2 sampleText2 @?=
              Right [ (WcParseLine { _wcFilename = $(mkAbsFile "/etc/passwd")
                                   , _wcLines         = Just 44
                                   , _wcWords         = Just 68
                                   , _wcChars         = Nothing
                                   , _wcBytes         = Nothing
                                   , _wcMaxLineLength = Nothing
                                   })
                    , (WcParseLine { _wcFilename = $(mkAbsFile "/etc/shells")
                                   , _wcLines         = Just 6
                                   , _wcWords         = Just 10
                                   , _wcChars         = Nothing
                                   , _wcBytes         = Nothing
                                   , _wcMaxLineLength = Nothing
                                   })
                    ]
        ]

------------------------------------------------------------

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = do
  _ ← runTestsP_ tests p
  return ()

tests ∷ TestTree
tests = testGroup "ProcLib.Wc" [ wcCmdTest, wcParseTest ]

-- that's all, folks! ---------------------------------------------------------
