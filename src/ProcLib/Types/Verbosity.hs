{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}


module ProcLib.Types.Verbosity
  ( -- | data structure to handle what things to be verbose about
    ShowCmd(..), ShowMockCmd(..)
  , ShowExec(..), ShowMockExec(..)
  , VerboseDetail(..), Verbosity( verbosity )
  , showCmd, showMockCmd, showExec, showMockExec
  , warnActionR
  )
where

-- base --------------------------------

import Control.Monad    ( (>>), return, when )
import Data.Eq          ( Eq, (==) )
import Data.Function    ( ($), (&), id )
import Numeric.Natural  ( Natural )
import Text.Show        ( Show )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.Setter  ( (.~) )
import Control.Lens.TH      ( makeLenses )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, warn )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.ProcIOAction  ( ProcIOAction(..) )

--------------------------------------------------------------------------------

data ShowCmd      = ShowCmd     | NoShowCmd
  deriving (Eq, Show)
data ShowMockCmd  = ShowMockCmd | NoShowMockCmd
  deriving (Eq, Show)
data ShowExec     = ShowExec     | NoShowExec
  deriving (Eq, Show)
data ShowMockExec = ShowMockExec | NoShowMockExec
  deriving (Eq, Show)

data VerboseDetail = VerboseDetail { _showCmd      :: ShowCmd
                                   , _showMockCmd  :: ShowMockCmd
                                   , _showExec     :: ShowExec
                                   , _showMockExec :: ShowMockExec
                                   }
  deriving (Eq, Show)

$( makeLenses ''VerboseDetail )

instance Default VerboseDetail where
  def = VerboseDetail NoShowCmd NoShowMockCmd NoShowExec NoShowMockExec

class Verbosity α where
  verbosity :: α -> VerboseDetail

instance Verbosity VerboseDetail where
  verbosity = id

------------------------------------------------------------

instance Verbosity Natural where
  verbosity 0 = def
  verbosity 1 = verbosity (0 :: Natural)
              & showCmd      .~ ShowCmd
              & showMockCmd  .~ ShowMockCmd
              & showExec     .~ ShowExec
              & showMockExec .~ ShowMockExec
  verbosity _ = verbosity (1 :: Natural)

------------------------------------------------------------

warnAction :: MonadIO μ => VerboseDetail -> ProcIOAction -> μ ()
warnAction v (DoIO c) =
  when (v ^. showCmd == ShowCmd) (warn $ [fmtT|IO    : %T|] c)
warnAction v (DoMockIO c) =
  when (v ^. showCmd == ShowCmd) (warn $ [fmtT|(IO)  : %T|] c)
warnAction v (DoCmd c) =
  when (v ^. showCmd == ShowCmd) (warn $ [fmtT|CMD   : %T|] c)
warnAction v (DoMockCmd c) =
  when (v ^. showCmd==ShowCmd)   (warn $ [fmtT|(CMD) : %T|] c)
warnAction v (DoExec c)     =
  when (v ^. showExec==ShowExec) (warn $ [fmtT|CMD>    %T|] c)
warnAction v (DoMockExec c) =
  when (v ^. showExec==ShowExec) (warn $ [fmtT|<CMD>   %T|] c)

warnActionR :: (Verbosity υ, MonadIO μ0) => υ -> ProcIOAction -> μ0 ProcIOAction
warnActionR v a = warnAction (verbosity v) a >> return a

-- that's all, folks! ----------------------------------------------------------
