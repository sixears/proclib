{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- change StdStream_ to take a phantom arg to say whether it's stdin, stdout
--   or stderr

module ProcLib.Types
  ( HostName, Port, UserName, hostName, port, userName )
where

import Prelude ( )

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON( parseJSON ), FromJSONKey
                         , ToJSON( toJSON ), ToJSONKey
                         , Value( String )
                         , typeMismatch
                         )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Eq        ( Eq )
import Data.Function  ( (.), ($) )
import Data.String    ( IsString( fromString ) )
import Data.Word      ( Word16 )
import Text.Show      ( Show, show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- hashable ----------------------------

import Data.Hashable  ( Hashable( hashWithSalt ) )

-- template-haskell --------------------

import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-------------------------------------------------------------------------------

newtype HostName = HostName Text
  deriving (Eq, FromJSONKey, ToJSONKey, Show)

instance IsString HostName where
  fromString = HostName . pack

instance Hashable HostName where
  hashWithSalt i (HostName h) = hashWithSalt i h

instance FromJSON HostName where
  parseJSON (String t) = return $ HostName t
  parseJSON invalid    = typeMismatch "HostName" invalid
instance ToJSON HostName where
  toJSON (HostName h) = String h

hostName :: Printable t => t -> HostName
hostName = HostName . toText

instance Printable HostName where
  print (HostName h) = P.text h

----------------------------------------

newtype UserName = UserName Text
  deriving (Eq, Lift, Show)

instance IsString UserName where
  fromString = UserName . pack

-- instance Lift UserName where
--   lift (UserName u) = sigE (litE (StringL $ unpack u)) (conT 'Text)

instance Printable UserName where
  print (UserName u) = P.text u

userName :: Printable t => t -> UserName
userName = UserName . toText

----------------------------------------

newtype Port = Port Word16
  deriving (Eq, Lift, Show)

-- instance Lift Port where
--   lift (Port p) = appE (conE 'Port) (lift p)

port :: Word16 -> Port
port = Port

instance Printable Port where
  print (Port p) = P.string $ show p

-- that's all, folks! ---------------------------------------------------------
