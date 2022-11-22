{-# LANGUAGE InstanceSigs #-}

module ProcLib.Types.ProcIOAction
  ( ProcIOAction(..) )
where

-- base --------------------------------

import Data.Eq    ( Eq )
import Text.Show  ( Show )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.CmdSpec  ( CmdSpec )

--------------------------------------------------------------------------------

data ProcIOAction = DoCmd      CmdSpec
                  | DoMockCmd  CmdSpec
                  | DoExec     CmdSpec
                  | DoMockExec CmdSpec
                  | DoIO       Text
                  | DoMockIO   Text
  deriving (Eq, Show)

-- that's all, folks! ----------------------------------------------------------
