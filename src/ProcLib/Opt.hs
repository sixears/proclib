{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module ProcLib.Opt
  ( Opt( checkOpt, checkOpts, cmpOpt, mkOpts ), OptError( OptError )
  , boolOpt, cmpOptDefaultLensed
  , cmpOptPrism, cmpOptPrisms
  , liftAbsFile, liftRelFile, liftRelFiles, liftText, liftTexts
  , listOpt, maybeOpt, maybeOptShow
  )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( (>>=), (=<<), mapM, msum, return )
import Data.Bool            ( Bool, (&&), (||), bool )
import Data.Either          ( Either( Left, Right ) )
import Data.Eq              ( Eq, (==) )
import Data.Foldable        ( any, concat )
import Data.Function        ( (.), ($) )
import Data.Functor         ( Functor, (<$>), fmap )
import Data.List            ( (++), tails )
import Data.Maybe           ( Maybe( Just, Nothing )
                            , fromJust, mapMaybe, maybe )
import Data.Tuple           ( uncurry )
import Text.Show            ( Show, show )

-- fmt ---------------------------------

import Text.Fmt  ( fmt )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( Getting, (^.) )
import Control.Lens.Prism   ( Prism' )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- safe --------------------------------

import Safe ( initSafe )

-- path --------------------------------

import Path  ( Abs, File, Path, Rel, parseAbsFile, parseRelFile, toFilePath )

-- template-haskell --------------------

import Language.Haskell.TH         ( Exp( AppE, ConE, LitE, VarE )
                                   , Lit( StringL )
                                   , Q
                                   , appE, conE, litE, listE, varE
                                   )
import Language.Haskell.TH.Syntax  ( Lift( lift ), Name )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

-------------------------------------------------------------------------------

-- | an error found with a command option, or with a combination of command
--   options; Text is a human-readable error message; [o] is the set of
--   options giving rise to the error
data OptError o = OptError Text [o]
  deriving Eq

instance Functor OptError where
  fmap f (OptError t os) = OptError t (fmap f os)

instance Show o => Show (OptError o) where
  show (OptError t os) = [fmt|OptError: %t [%L]|] t (fmap show os)

-------------------------------------------------------------------------------

class (Lift o, Show o) => Opt o where
  -- have a one-by-one checkOpt, and also a full-in checkOpts_ that can check
  -- for combinations, which combine to
  -- a checkOpts which by default lets everything happen
  -- provide a standard no-duplicate-options fn

  -- | This is the checking entry point used to check all options in a list.
  --   Particularly, this is used by mkOpts
  checkOpts :: MonadError (OptError o) m => [o] -> m [o]
  checkOpts os = mapM checkOpt os >>= checkPairs' >>= checkOpts_
                 where checkPairs' xs = case checkPairs xs of
                                          []    -> return xs
                                          e : _ -> throwError e

  -- | override this to check individual options for sanity
  checkOpt :: MonadError (OptError o) m => o -> m o
  checkOpt = return

  -- | override this to make checks across a list of options
  checkOpts_ :: MonadError (OptError o) m => [o] -> m [o]
  -- default implementation passes all things
  checkOpts_ = return

  -- | check each pair of options for conflict, return a list of errors
  checkPairs :: [o] -> [OptError o]
  checkPairs = mapMaybe (uncurry cmpOpt) . pairs

  -- | Compare two options to see if they are legit to co-exist within an
  --   option set.  E.g., you might want to return a Just OptError if
  --   you have two options setting output multiplier and therefore confusion
  --   about which applies.  Return Nothing if the options do not conflict.
  --   used by the default implementation of checkPairs
  cmpOpt :: o -> o -> Maybe (OptError o)

  -- default implementation of cmpOpt just compares two options for equality;
  -- if they are equal, an error is returned
  default cmpOpt :: Eq o => o -> o -> Maybe (OptError o)
  cmpOpt = cmpOptDefault

  -- | splice this in with TemplateHaskell to get compile-time options checking
  mkOpts :: [o] -> Q Exp
  mkOpts os = case checkOpts os of
                Left  e -> error (show e)
                Right os' -> [| os' |]

-- | this is the default implementation of cmpOpt for Eq instances; given its
--   own name so instances overriding cmpOpt can use this as part of their
--   implementation. For lack of confusion, you probably don't want to
--   override this fn.
--   This implementation should work for any simple unvalued data constructor,
--   but for any data constructor that takes a value, it will throw an error
--   only if the value(s) to the constructor are themselves equal (==).
cmpOptDefault :: Eq o => o -> o -> Maybe (OptError o)
cmpOptDefault o o' = bool Nothing
                          (Just (OptError "duplicate options found " [o]))
                          (o == o')

-------------------------------------------------------------------------------

-- | a checker of options, based on lenses; this checker will return an error
--   whenever both options match any lens in the input list; the paired
--   name should be something identifying the type of option (it is used to
--   construct the error message).  If none of the lenses match both args, then
--   Nothing is returned.

-- | like `cmpOptDefaultLensed`, but if the none of lenses lead to an error,
--   then the two args are compared using `cmpOptDefault`.
cmpOptDefaultLensed :: Eq o
                    => --   some text to use in the raised error, if any.
                       --   suggest some identifier of the opt type to
                       --   help identify
                       Text
                       --   if both objects match a lens in this list,
                       --   then an OptError is returned using the given
                       --   text
                    -> [(o -> Bool, Text)]
                       --   lens comparators. If, of the pair of objects,
                       --   the first matches a lens from the first list
                       --   and second matches a lens from the second list,
                       --   then an OptError is returned using the given
                       --   text
                    -> [([o -> Bool], [o -> Bool], Text)]
                    -> o -> o
                    -> Maybe (OptError o)
cmpOptDefaultLensed tname singleLenses dualLenses =
  cmpOptDefaultLensed__  cmpOptDefault tname
                         (fmap dualify singleLenses ++ dualLenses)
  where dualify (l,t) = ([l],[l],t)
  
cmpOptDefaultLensed__ :: (o -> o -> Maybe (OptError o))
                      -> Text
                      -> [([o -> Bool], [o -> Bool], Text)]
                      -> o
                      -> o
                      -> Maybe (OptError o)
cmpOptDefaultLensed__ e tname lenses o o' =
    let multiOptError = [fmt|clashing uses of %t:%t options|] tname        
        -- do any of the first lenses match the first o, and do any of the
        -- second lenses match the second o
        f (ls,ls',s) = if    any ($ o) ls  && any ($ o') ls'
                          || any ($ o) ls' && any ($ o') ls
                       then Just (OptError (multiOptError s) [o,o'])
                       else e o o'
        errors  =  fmap f lenses
     in msum errors

-------------------------------------------------------------------------------

-- | Utility to help creating cmpOpt instances for algebraic types that are
--   unions of other opts; by looking into prisms.  Given a prism and a
--   convertor, check if both objects match the prism and if so, use cmpOpt
--   on the thus focussed values; else return nothing
cmpOptPrism :: Opt a => Prism' s a -> (a -> b) -> s -> s -> Maybe (OptError b)
cmpOptPrism l m x y = case (x ^? l, y ^? l) of
              (Just x', Just y') -> case cmpOpt x' y' of
                                      Just e  -> Just (m <$> e)
                                      Nothing -> Nothing
              _                  -> Nothing

-- | Utility to help creating cmpOpt instances for algebraic types by checking
--   opts against lenses, and then cmpOpt the focussed instances.  Well,
--   that's how it's meant to be used, anyway.
cmpOptPrisms :: [x -> y -> Maybe a] -> x -> y -> Maybe a
cmpOptPrisms xs x y = msum $ fmap ( \ f -> f x y ) xs

-------------------------------------------------------------------------------

-- | generate a list of pairs of elements from the list; each pair by position
--   in the list is generated only once.  No guarantee about the order of the
--   output list is made.
--   e.g., pairs [9,8,8,7] -> [(9,8), (9,8), (9,7), (8,8), (8,7), (8,7)]
pairs :: [b] -> [(b, b)]
pairs =
  -- _ case is never used; but makes the compiler happy (and is safe because
  -- of the concat)
  concat . fmap (\case x:ys -> fmap ((,) x) ys; _ -> [])
         . initSafe . initSafe . tails

-------------------------------------------------------------------------------

-- | helper for lift implementations using a simple path c'tor, e.g.,
--   lift (DU_PATH p) = liftPathCtor 'DU_PATH p

liftAbsFile :: Name -> Path Abs File -> Q Exp
liftAbsFile name p =
    let f = toFilePath p
     in return $ AppE (ConE name)
                      (AppE (VarE 'fromJust) (AppE (VarE 'parseAbsFile)
                                                   (LitE . StringL $ f)))

----------------------------------------

liftRelFile :: Name -> Path Rel File -> Q Exp
liftRelFile = _liftPath 'parseRelFile

liftRelFile' :: Path Rel File -> Q Exp
liftRelFile' = _liftPath' 'parseRelFile

liftRelFiles :: Name -> [Path Rel File] -> Q Exp
liftRelFiles name fns = appE (conE name) (listE (fmap liftRelFile' fns))


----------------------------------------

_liftPath :: Name -> Name -> Path b t -> Q Exp
_liftPath parser name p = appE (conE name) (_liftPath' parser p)

----------------------------------------

_liftPath' :: Name -> Path b t -> Q Exp
_liftPath' parser p =
  let f = toFilePath p
   in appE (varE 'fromJust) (appE (varE parser) (litE . StringL $ f))

-------------------------------------------------------------------------------

-- | lift a Text value into Q space

liftText :: Text -> Q Exp
liftText = appE (varE 'pack) . lift . unpack

-------------------------------------------------------------------------------

-- | lift a [Text] value into Q space

liftTexts :: [Text] -> Q Exp
liftTexts = appE (appE (varE 'fmap) (varE 'pack)) . lift . fmap unpack

-------------------------------------------------------------------------------

-- | Create option text based on a Maybe field in a req.
--   E.g., texts = maybeOpt req sshPort "-p" portNumberToText
--   will produce a non-empty texts, being
--     [ "-p", portNumberToText (fromJust $ req ^. sshPort) ]
--   iff req ^. sshPort is not Nothing

maybeOpt :: s -> Getting (Maybe a) s (Maybe a) -> Text -> (a -> Text) -> [Text]
maybeOpt req lens arg f = maybe [] ((arg:) . pure . f) (req ^. lens)

maybeOptShow :: Show a => s -> Getting (Maybe a) s (Maybe a) -> Text -> [Text]
maybeOptShow req lens arg = maybeOpt req lens arg (pack . show)

-------------------------------------------------------------------------------

-- | Create option text based on a Bool field in a req.

boolOpt :: s -> Getting Bool s Bool -> Text -> [Text]
boolOpt req lens arg = bool [] [arg] (req ^. lens)

-------------------------------------------------------------------------------

-- | Create option text by prepending each (transformed) value with
--   some text.
--   E.g.,
--     listOpt req rsyncExcludes "--exclude" toFilePathT

listOpt :: s -> Getting [a] s [a] -> Text -> (a -> Text) -> [Text]
listOpt req lens arg f = ((arg:) . pure) =<< fmap f (req ^. lens)

-- that's all, folks! ---------------------------------------------------------
