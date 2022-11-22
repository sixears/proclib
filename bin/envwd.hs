{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- base --------------------------------

import Control.Monad           ( forM_, join, return )
import Control.Monad.IO.Class  ( liftIO )
import Data.Either             ( Either, either )
import Data.Function           ( (.), ($), (&) )
import Data.Functor            ( fmap )
import Data.List               ( sort )
import System.IO               ( IO, print )
import Text.Show               ( Show )

-- fluffy ------------------------------

import MonadError  ( splitMError )
import Natural         ( One, Two )
import OptParsePlus     ( parseOpts )

-- lens --------------------------------

import Control.Lens.Setter  ( (.~) )
import Control.Lens.TH      ( makeLenses )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( progDesc )

-- proclib -----------------------------

import qualified  ProcLib.Paths  as  Paths

import ProcLib.CommonOpt.DryRun       ( DryRunLevel
                                      , HasDryRunLevel( dryRunLevel )
                                      , dryRun2P
                                      )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel( verboseLevel )
                                      , VerboseLevel, verboseP )
import ProcLib.Error.ExecCreateError  ( ExecCreateError )
import ProcLib.Process                ( doProcIO, mkIO, mkIO', mkProc'_
                                      , mkProc_ )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.CreateProcOpts   ( MockLvl( MockLvl ), defCPOpts, mockLvl )
import ProcLib.Types.ProcIO           ( ProcIO )

-- text --------------------------------

import Data.Text     ( Text )
import Data.Text.IO  ( putStrLn )

--------------------------------------------------------------------------------

env :: (MonadError ExecCreateError η) => ProcIO ExecCreateError η [Text]
env = mkProc_ $ CmdSpec Paths.env []

pwd :: (MonadError ExecCreateError η) => ProcIO ExecCreateError η Text
pwd = mkProc'_ (defCPOpts & mockLvl .~ 2) $ CmdSpec Paths.pwd []

handleE :: (Show ε) => Either ε () -> IO ()
handleE = either print return

data Options = Options { _dryRunL  :: DryRunLevel  Two
                       , _verboseL :: VerboseLevel One }
$( makeLenses ''Options )

instance HasVerboseLevel One Options where
  verboseLevel = verboseL

instance HasDryRunLevel Two Options where
  dryRunLevel = dryRunL

main :: IO ()
main = do
  opts <- parseOpts (progDesc "show env, pwd")
                    (Options ⊳ dryRun2P ⊵ verboseP)
  join . fmap handleE . splitMError . doProcIO opts $ do
    p <- pwd
    es <- env
    mkIO "print sorted env" (liftIO $ forM_ (sort es) putStrLn)
    mkIO' (MockLvl 2) () "print pwd" (liftIO $ putStrLn p)

-- that's all, folks! ----------------------------
