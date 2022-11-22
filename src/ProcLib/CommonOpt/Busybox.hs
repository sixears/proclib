-- see README.md

module ProcLib.CommonOpt.Busybox
  ( AsBusyboxOpt(..), BusyboxOpt(..), HasUseBusybox(..)
  , HasROUseBusybox(..), UseBusybox(..)
  , androidBusyboxBin, androidBusyboxDir, boolBBOpt, busybox
  , ifUseBusybox, maybeBBOpt, mebbeUseBusybox
  , useAsBusyboxOpt, useAsBusyboxOpts
  )
where

import Prelude ()

-- base --------------------------------

import Data.Bool      ( Bool )
import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq, (==) )
import Data.Foldable  ( Foldable, toList )
import Data.Function  ( (.), ($), const, id )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Text.Show      ( Show )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( Getter, Getting, (^.), to )
import Control.Lens.Lens    ( Lens' )
import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- path --------------------------------

import Path  ( Abs, Dir, File, Path, Rel, (</>), mkAbsDir )

-- template-haskell --------------------

import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Opt  ( Opt, boolOpt, maybeOpt )

-------------------------------------------------------------------------------

data BusyboxOpt = ANDROID_BUSYBOX
  deriving (Eq, Lift, Show)

instance Opt BusyboxOpt where

------------------------------------------------------------

class AsBusyboxOpt o where
  _BusyboxOpt :: Prism' o BusyboxOpt
  _ANDROID_BUSYBOX :: Prism' o ()
  _ANDROID_BUSYBOX = _BusyboxOpt . _ANDROID_BUSYBOX

instance AsBusyboxOpt BusyboxOpt where
  _BusyboxOpt = id
  _ANDROID_BUSYBOX = prism (const ANDROID_BUSYBOX) (const $ Right ())

busybox :: AsBusyboxOpt o => o
busybox = _ANDROID_BUSYBOX # ()

------------------------------------------------------------

data UseBusybox = UseBusybox | NoUseBusybox
  deriving (Eq, Show)

class HasUseBusybox h where
  useBusybox :: Lens' h UseBusybox

class HasROUseBusybox h where
  useBusyboxRO :: Getter h UseBusybox

instance HasUseBusybox UseBusybox where
  useBusybox = id

instance HasROUseBusybox UseBusybox where
  useBusyboxRO = id

useAsBusyboxOpt :: AsBusyboxOpt x => Getter x UseBusybox
useAsBusyboxOpt = to go
                  where go x = case (x ^? _ANDROID_BUSYBOX) of
                                 Just _  -> UseBusybox
                                 Nothing -> NoUseBusybox

-- useAsBusyboxOpts :: AsBusyboxOpt x => Getter [x] UseBusybox
-- useAsBusyboxOpts :: (Contravariant f, AsBusyboxOpt x, Foldable t) =>
--                     (UseBusybox -> f UseBusybox) -> (t x) -> f (t x)
useAsBusyboxOpts :: (AsBusyboxOpt x, Foldable φ) => Getter (φ x) UseBusybox
useAsBusyboxOpts = to (goes . toList)
                   where go x = case (x ^? _ANDROID_BUSYBOX) of
                                  Just _  -> UseBusybox
                                  Nothing -> NoUseBusybox
                         goes []       = NoUseBusybox
                         goes (x : xs) = if go x == UseBusybox
                                         then UseBusybox
                                         else goes xs

ifUseBusybox :: HasROUseBusybox h => h -> a -> a -> a
ifUseBusybox b t e = if b ^. useBusyboxRO == UseBusybox then t else e

mebbeUseBusybox :: (AsBusyboxOpt p, HasROUseBusybox h) => h -> [p]
mebbeUseBusybox h = ifUseBusybox h [busybox] []

-------------------------------------------------------------------------------

androidBusyboxDir :: Path Abs Dir
androidBusyboxDir = $( mkAbsDir "/system/xbin" )

----------------------------------------

-- | a named file withn the android busybox dir
androidBusyboxBin :: Path Rel File -> Path Abs File
androidBusyboxBin f = androidBusyboxDir </> f

-------------------------------------------------------------------------------

-- | if the (second) lens (to a Bool) refers to a true value, then insert the
--   given text --- first value is the normal case, second value is the
--   busybox case (used if the (first) lens, which is to 'busybox-ness',
--   returns true)
--
--   E.g., boolBBOpt req du.BusyBoxOpts Req.duShowFiles "--all" "-a"
boolBBOpt :: s -> Getting Bool s Bool -> Getting Bool s Bool -> Text -> Text
          -> [Text]

boolBBOpt req isBB lens arg bbArg =
  boolOpt req lens (if req ^. isBB then bbArg else arg)

-------------------------------------------------------------------------------

-- | if the (second) lens refers to a non-Nothing value, then insert the
--   given text --- first value is the normal case, second value is the
--   busybox case (used if the (first) lens, which is to 'busybox-ness',
--   returns true); followed by the Just value of the lens (that is, with the
--   Just stripped), which is passed through the given function to produce
--   a Text representation
--
--   E.g, maybeBBOpt req Req.duBusyBoxOpts Req.duLimitDepth "--max-depth" "-d"
--                   (pack . show)
maybeBBOpt :: s -> Getting Bool s Bool -> Getting (Maybe a) s (Maybe a)
           -> Text -> Text -> (a -> Text) -> [Text]
maybeBBOpt req isBB lens arg bbArg =
  maybeOpt req lens (if req ^. isBB then bbArg else arg)

-- that's all, folks! ---------------------------------------------------------

