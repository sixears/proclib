{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TemplateHaskell           #-}

module ProcLib.Types.CreateProcOpts
  ( CreateGroup(..), CreateProcOpts, HasCreateProcOpts( createProcOpts )
  , HasMockLvl( mockLvl ), MockLvl( MockLvl )
  , cmdName, createGroup, cwd, defCPOpts, defCreateProcOpts, defMockLvl, env
  , inH, mock, withInS, withInT
  )
where

-- base --------------------------------

import Data.Eq                ( Eq )
import Data.Function          ( (.), (&), id )
import Data.Functor           ( fmap )
import Data.Maybe             ( Maybe( Nothing ) )
import Data.Monoid            ( (<>) )
import Numeric.Natural        ( Natural )
import Text.Show              ( Show( show ) )

-- data-default ------------------------

import Data.Default  ( Default, def )

-- env-plus ------------------------------

import Env.Types  ( Env )

-- fpath -------------------------------

import FPath.AbsDir  ( AbsDir )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Review  ( (#) )
import Control.Lens.Setter  ( (.~) )

-- process -----------------------------

import System.Process  ( StdStream( NoStream ) )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.MkStream     ( MkStream, _MkS, _MkT )
import ProcLib.Types.MockDefault  ( MockDefault( mockDef ) )

--------------------------------------------------------------------------------

-- CreateGroup ---------------------------------------------

data CreateGroup = CreateGroup | NoCreateGroup
  deriving (Eq, Show)

-- HasMockLvl --------------------------

class HasMockLvl γ where
  mockLvl :: Lens' γ Natural

newtype MockLvl = MockLvl { unMockLvl :: Natural }

instance HasMockLvl MockLvl where
  mockLvl = lens unMockLvl (\ _ l -> MockLvl l)

defMockLvl :: MockLvl
defMockLvl = MockLvl 1

instance Default MockLvl where
  def = defMockLvl

-- CreateProcOpts ------------------------------------------

data CreateProcOpts ω = CreateProcOpts { _cwd          :: Maybe AbsDir
                                       , _env          :: Maybe Env
                                       , _createGroup  :: CreateGroup
                                       -- function name (for error messages)
                                       -- NOT the executable name
                                       -- see System.Process.createProcess_
                                       , _cmdName      :: Maybe Text
                                       , _mock         :: ω
                                       , _mockLvl      :: Natural
                                       , _inH          :: MkStream
                                       }

class HasCreateProcOpts c ω | c -> ω where
      createProcOpts :: Lens' c (CreateProcOpts ω)
      {-# INLINE cmdName #-}
      cmdName :: Lens' c (Maybe Text)
      {-# INLINE createGroup #-}
      createGroup :: Lens' c CreateGroup
      {-# INLINE cwd #-}
      cwd :: Lens' c (Maybe AbsDir)
      {-# INLINE env #-}
      env :: Lens' c (Maybe Env)
      {-# INLINE inH #-}
      inH :: Lens' c MkStream
      {-# INLINE mock #-}
      mock :: Lens' c ω
      cmdName     = createProcOpts . cmdName
      createGroup = createProcOpts . createGroup
      cwd         = createProcOpts . cwd
      env         = createProcOpts . env
      inH         = createProcOpts . inH
      mock        = createProcOpts . mock

instance HasMockLvl (CreateProcOpts ω) where
  {-# INLINE mockLvl #-}
  mockLvl f cgo = fmap ( \ ml -> cgo { _mockLvl = ml }) (f (_mockLvl cgo))

instance HasCreateProcOpts (CreateProcOpts ω) ω where
  {-# INLINE createProcOpts #-}
  createProcOpts = id
  {-# INLINE cmdName #-}
  cmdName f cgo = fmap ( \ nm  -> cgo { _cmdName = nm  }) (f (_cmdName cgo))
  {-# INLINE createGroup #-}
  createGroup f cgo =
    fmap (\ cg -> cgo { _createGroup = cg }) (f (_createGroup cgo))
  {-# INLINE cwd #-}
  cwd     f cgo = fmap ( \ wd  -> cgo { _cwd     = wd  }) (f (_cwd     cgo))
  {-# INLINE env #-}
  env     f cgo = fmap ( \ nv  -> cgo { _env     = nv  }) (f (_env     cgo))
  {-# INLINE inH #-}
  inH     f cgo = fmap ( \ inh -> cgo { _inH     = inh }) (f (_inH     cgo))
  {-# INLINE mock #-}
  mock    f cgo = fmap ( \ mck -> cgo { _mock    = mck }) (f (_mock    cgo))

instance Show ω => Show (CreateProcOpts ω) where
  show o =  "CreateProcOpts cwd: " <> show (_cwd o) <> "\n"
         <> "               inH: " <> show (_inH o)

defCreateProcOpts :: MockDefault ω => CreateProcOpts ω
defCreateProcOpts = CreateProcOpts { _cwd = Nothing
                                   , _env = Nothing
                                   , _createGroup = NoCreateGroup
                                   , _cmdName  = Nothing
                                   , _mock     = mockDef
                                   , _mockLvl = 1
                                   , _inH      = _MkS # NoStream
                                   }

-- | default CreateProcOpts, shorter name for convenience
defCPOpts :: MockDefault ω => CreateProcOpts ω
defCPOpts = defCreateProcOpts

instance MockDefault ω => Default (CreateProcOpts ω) where
  def = defCreateProcOpts

withInS :: HasCreateProcOpts ψ ω => ψ -> StdStream -> ψ
withInS c s = c & inH .~ (_MkS # s)

withInT :: HasCreateProcOpts ψ ω => ψ -> Text -> ψ
withInT c t = c & inH .~ (_MkT # t)


-- that's all, folks! ----------------------------------------------------------
