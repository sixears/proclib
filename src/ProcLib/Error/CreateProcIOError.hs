{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UnicodeSyntax         #-}

module ProcLib.Error.CreateProcIOError
  ( CreateProcIOError(..), ExecCreatePathIOParseError
  , cpiCreateE', ecpipExecE', ecpipCreateE' )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), (&) )
import GHC.Stack          ( HasCallStack, callStack )
import Text.Show          ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fpath -------------------------------

import FPath.Error.FPathError    ( AsFPathError( _FPathError ), FPathError )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens  ( lens )
import Control.Lens.TH    ( makePrisms )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣), (⊢) )

-- parsec-plus ------------------------------

import ParsecPlus  ( AsParseError( _ParseError ), ParseError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.ExecError        ( AsExecError( _ExecError ), ExecError
                                      , ToMaybeTexts, asExecError )
import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError( CreateProcError )
                                      )
import ProcLib.Types.CmdSpec          ( CmdSpec )
import ProcLib.Types.ExitVal          ( ExitVal )

--------------------------------------------------------------------------------

-- | encompassing both an ExecError, and a CreateProcError

data CreateProcIOError = CPIIOE IOError | CPICreateE CreateProcError
  deriving (Eq, Show)

$( makePrisms ''CreateProcIOError )

instance Exception CreateProcIOError

instance Printable CreateProcIOError where
  print (CPIIOE e)     = print e
  print (CPICreateE e) = print e

instance HasCallstack CreateProcIOError where
  callstack = lens (\ case CPIIOE     ioe → ioe ⊣ callstack
                           CPICreateE cpe → cpe ⊣ callstack)
                   (\ cpioe cs → case cpioe of
                                   CPIIOE ioe →
                                     CPIIOE $ ioe & callstack ⊢ cs
                                   CPICreateE cpe →
                                     CPICreateE $ cpe & callstack ⊢ cs
                   )

instance AsIOError CreateProcIOError where
  _IOError = _CPIIOE

instance AsCreateProcError CreateProcIOError where
  _CreateProcError = _CPICreateE

cpiCreateE' ∷ HasCallStack ⇒ IOError → CreateProcIOError
cpiCreateE' ioe = CPICreateE $ CreateProcError ioe callStack

------------------------------------------------------------

data ExecCreatePathIOParseError = ECPIOPExecE ExecError
                                | ECPIOPCreateE CreateProcError
                                | ECPIOPPathE FPathError
                                | ECPIOPE IOError
                                | ECPIOPParseE ParseError
  deriving (Eq, Show)

$( makePrisms ''ExecCreatePathIOParseError )

instance AsExecError ExecCreatePathIOParseError where
  _ExecError = _ECPIOPExecE

instance AsCreateProcError ExecCreatePathIOParseError where
  _CreateProcError = _ECPIOPCreateE

instance AsFPathError ExecCreatePathIOParseError where
  _FPathError = _ECPIOPPathE

instance AsIOError ExecCreatePathIOParseError where
  _IOError = _ECPIOPE

instance AsParseError ExecCreatePathIOParseError where
  _ParseError = _ECPIOPParseE

ecpipExecE' ∷ ToMaybeTexts ω ⇒ CmdSpec → ExitVal → ω → ExecCreatePathIOParseError
ecpipExecE' cmdspc xtvl x = ECPIOPExecE $ asExecError cmdspc xtvl x

ecpipCreateE' ∷ HasCallStack ⇒ IOError → ExecCreatePathIOParseError
ecpipCreateE' ioe =  ECPIOPCreateE $ CreateProcError ioe callStack

instance Printable ExecCreatePathIOParseError where
  print (ECPIOPExecE e)   = print e
  print (ECPIOPCreateE e) = P.string $ show e
  print (ECPIOPPathE e)   = P.string $ show e
  print (ECPIOPE e)       = P.string $ show e
  print (ECPIOPParseE e)  = P.string $ show e

-- that's all, folks! ----------------------------------------------------------
