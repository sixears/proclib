module ProcLib.Error.ExecCreateError
  ( ExecCreateError(..), ExecCreateIOError(..), ExecCreatePathError(..)
  , ExecCreatePathIOError(..)
  , _ECCreateE, _ECExecE, _ECICreateE, _ECIExecE, _ECIIOE
  , _ECPCreateE, _ECPExecE, _ECPPathE
  , _ECPIOCreateE, _ECPIOExecE, _ECPIOPathE, _ECPIOE
  , ecCreateE', ecExecE', eciCreateE', eciExecE', ecpiExecE', ecpiCreateE'
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), (&) )
import GHC.Stack          ( HasCallStack, callStack )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ), FPathError )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens  ( lens )
import Control.Lens.TH    ( makePrisms )

-- monaderror-io -----------------------

import MonadError.IO.Error    ( AsIOError( _IOError ), IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣), (⊢) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError( CreateProcError )
                                      )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError )
                                      , ExecError
                                      , ToMaybeTexts
                                      , asExecError
                                      )
import ProcLib.Types.CmdSpec          ( CmdSpec )
import ProcLib.Types.ExitVal          ( ExitVal )

--------------------------------------------------------------------------------

-- | encompassing both an ExecError, and a CreateProcError

data ExecCreateError = ECExecE   ExecError
                     | ECCreateE CreateProcError
  deriving (Eq, Show)

$( makePrisms ''ExecCreateError )

--------------------

instance Exception ExecCreateError

--------------------

instance HasCallstack ExecCreateError where
  callstack = lens (\ case ECExecE   ee  → ee  ⊣ callstack
                           ECCreateE cpe → cpe ⊣ callstack)
                   (\ ece cs → case ece of (ECExecE ee) →
                                             ECExecE $ ee & callstack ⊢ cs
                                           (ECCreateE cpe) →
                                             ECCreateE $ cpe & callstack ⊢ cs
                   )

--------------------

instance Printable ExecCreateError where
  print (ECExecE   ee)  = print ee
  print (ECCreateE cpe) = print cpe

--------------------

instance AsExecError ExecCreateError where
  _ExecError = _ECExecE

instance AsCreateProcError ExecCreateError where
  _CreateProcError = _ECCreateE

ecExecE' ∷ ToMaybeTexts ω ⇒ CmdSpec → ExitVal → ω → ExecCreateError
ecExecE' cmdspc xtvl = ECExecE ∘ asExecError cmdspc xtvl

ecCreateE' ∷ HasCallStack ⇒ IOError → ExecCreateError
ecCreateE' ioe = ECCreateE $ CreateProcError ioe callStack

------------------------------------------------------------

data ExecCreateIOError = ECIExecE ExecError | ECICreateE CreateProcError
                       | ECIIOE IOError
  deriving (Eq, Show)

--------------------

$( makePrisms ''ExecCreateIOError )

--------------------

instance Exception ExecCreateIOError

--------------------

instance HasCallstack ExecCreateIOError where
  callstack = lens (\ case ECIExecE   ee  → ee  ⊣ callstack
                           ECICreateE cpe → cpe ⊣ callstack
                           ECIIOE     ioe → ioe ⊣ callstack
                   )
                   (\ ecioe cs →
                      case ecioe of
                        ECIExecE   ee  → ECIExecE   $ ee  & callstack ⊢ cs
                        ECICreateE cpe → ECICreateE $ cpe & callstack ⊢ cs
                        ECIIOE     ioe → ECIIOE     $ ioe & callstack ⊢ cs
                   )

--------------------

instance AsExecError ExecCreateIOError where
  _ExecError = _ECIExecE

--------------------

instance AsCreateProcError ExecCreateIOError where
  _CreateProcError = _ECICreateE

--------------------

instance AsIOError ExecCreateIOError where
  _IOError = _ECIIOE

--------------------

eciExecE' ∷ ToMaybeTexts ω ⇒ CmdSpec → ExitVal → ω → ExecCreateIOError
eciExecE' cmdspc xtvl x = ECIExecE $ asExecError cmdspc xtvl x

--------------------

eciCreateE' ∷ HasCallStack ⇒ IOError → ExecCreateIOError
eciCreateE' ioe =  ECICreateE $ CreateProcError ioe callStack

------------------------------------------------------------

data ExecCreatePathError = ECPExecE ExecError | ECPCreateE CreateProcError
                         | ECPPathE FPathError
  deriving (Eq, Show)

$( makePrisms ''ExecCreatePathError )

instance Exception ExecCreatePathError

--------------------

instance HasCallstack ExecCreatePathError where
  callstack = lens (\ case ECPExecE   ee  → ee  ⊣ callstack
                           ECPCreateE cpe → cpe ⊣ callstack
                           ECPPathE   fpe → fpe ⊣ callstack
                   )
                   (\ ecioe cs →
                      case ecioe of
                        ECPExecE   ee  → ECPExecE   $ ee  & callstack ⊢ cs
                        ECPCreateE cpe → ECPCreateE $ cpe & callstack ⊢ cs
                        ECPPathE   fpe → ECPPathE   $ fpe & callstack ⊢ cs
                   )

--------------------

instance AsExecError ExecCreatePathError where
  _ExecError = _ECPExecE

instance AsCreateProcError ExecCreatePathError where
  _CreateProcError = _ECPCreateE

instance AsFPathError ExecCreatePathError where
  _FPathError = _ECPPathE

------------------------------------------------------------

data ExecCreatePathIOError = ECPIOExecE ExecError | ECPIOCreateE CreateProcError
                           | ECPIOPathE FPathError | ECPIOE IOError
  deriving (Eq, Show)

--------------------

$( makePrisms ''ExecCreatePathIOError )

--------------------

instance Exception ExecCreatePathIOError

--------------------

instance HasCallstack ExecCreatePathIOError where
  callstack = lens (\ case ECPIOExecE   ee  → ee  ⊣ callstack
                           ECPIOCreateE cpe → cpe ⊣ callstack
                           ECPIOPathE   fpe → fpe ⊣ callstack
                           ECPIOE       ioe → ioe ⊣ callstack
                   )
                   (\ ecioe cs →
                      case ecioe of
                        ECPIOExecE   ee  → ECPIOExecE   $ ee  & callstack ⊢ cs
                        ECPIOCreateE cpe → ECPIOCreateE $ cpe & callstack ⊢ cs
                        ECPIOPathE   fpe → ECPIOPathE   $ fpe & callstack ⊢ cs
                        ECPIOE       ioe → ECPIOE       $ ioe & callstack ⊢ cs
                   )

--------------------

--------------------

instance AsExecError ExecCreatePathIOError where
  _ExecError = _ECPIOExecE

instance AsCreateProcError ExecCreatePathIOError where
  _CreateProcError = _ECPIOCreateE

instance AsFPathError ExecCreatePathIOError where
  _FPathError = _ECPIOPathE

instance AsIOError ExecCreatePathIOError where
  _IOError = _ECPIOE

ecpiExecE' ∷ ToMaybeTexts ω ⇒ CmdSpec → ExitVal → ω → ExecCreatePathIOError
ecpiExecE' cmdspc xtvl x = ECPIOExecE $ asExecError cmdspc xtvl x

ecpiCreateE' ∷ HasCallStack ⇒ IOError → ExecCreatePathIOError
ecpiCreateE' ioe =  ECPIOCreateE $ CreateProcError ioe callStack

-- that's all, folks! ----------------------------------------------------------
