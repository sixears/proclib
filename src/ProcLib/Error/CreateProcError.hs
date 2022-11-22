module ProcLib.Error.CreateProcError
  ( AsCreateProcError(..), CreateProcError(..) )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( (.), id )
import Data.Maybe         ( Maybe( Just ) )
import GHC.Stack          ( CallStack, callStack )
import Text.Show          ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )

-- monaderror-io -----------------------

import MonadError.IO.Error   ( AsIOError( _IOError {- , _IOErr -} ), IOError )

--------------------------------------------------------------------------------

data CreateProcError = CreateProcError IOError CallStack

instance Exception CreateProcError

instance Eq CreateProcError where
  CreateProcError ioe1 _ == CreateProcError ioe2 _ = ioe1 == ioe2

instance HasCallstack CreateProcError where
  callstack = lens (\ (CreateProcError _ cs) → cs)
                   (\ (CreateProcError ioe _) cs → CreateProcError ioe cs)

instance Printable CreateProcError where
  print (CreateProcError ioe _) = print ioe

instance Show CreateProcError where
  show (CreateProcError ioe _) = show ioe

instance AsIOError CreateProcError where
  _IOError = prism' (\ ioe → CreateProcError ioe callStack)
                    (\ (CreateProcError ioe _) → Just ioe)

------------------------------------------------------------

class AsCreateProcError ε where
  _CreateProcError :: Prism' ε CreateProcError
  _CreateProcErr   :: Prism' ε IOError
  _CreateProcErr   =  _CreateProcError . _IOError

instance AsCreateProcError CreateProcError where
  _CreateProcError = id

-- that's all, folks! ----------------------------------------------------------
