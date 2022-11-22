{-# LANGUAGE FlexibleInstances #-}

module ProcLib.ShellPath
  ( ShellPath, ShellPathFragment( SPathF, WildStar ), ShellPathLike
  , shellQuote, textPath )
where

import Prelude ( id )

-- base --------------------------------

import Data.Bool           ( (||) )
import Data.Char           ( isAlphaNum )
import Data.Foldable       ( elem )
import Data.Function       ( (.) )
import Data.Functor        ( fmap )
import Data.List.NonEmpty  ( NonEmpty )
import Data.String         ( String )
import GHC.Exts            ( toList )

-- path --------------------------------

import Path  ( Path, toFilePath )

-- text --------------------------------

import Data.Text  ( Text, concat, concatMap, cons, pack, singleton )

-------------------------------------------------------------------------------

-- | a bit of a shell path; e.g., a textual path, or a wildcard ('*', '?')
--   or maybe a character set
data ShellPathFragment = SPathF Text
                       | WildStar    -- asterisk

shellPathFragmentRender :: ShellPathFragment -> Text
shellPathFragmentRender (SPathF t) = t
shellPathFragmentRender WildStar   = singleton '*'

type ShellPath = NonEmpty ShellPathFragment

class ShellPathLike s where
  textPath   :: s -> Text
  -- | quote a string value to protect it from shell expansion & word splitting
  shellQuote :: s -> Text

instance ShellPathLike Text where
  textPath = id
  shellQuote = concatMap q
               where q c = if isAlphaNum c || c `elem` ("/_" :: String)
                           then singleton c
                           else '\\' `cons` singleton c

instance ShellPathLike ShellPath where
  textPath = concat . toList . fmap shellPathFragmentRender
  shellQuote = concat . toList . fmap quote
               where quote (SPathF t) = shellQuote t
                     quote WildStar   = singleton '*'

instance ShellPathLike (Path b t) where
  textPath = pack . toFilePath
  shellQuote = shellQuote . textPath

{-
instance ShellPathLike AbsPath where
  textPath   (AbsD d) = textPath d
  textPath   (AbsF f) = textPath f
  shellQuote (AbsD d) = shellQuote d
  shellQuote (AbsF f) = shellQuote f
-}

-- that's all, folks! ---------------------------------------------------------
