{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Moonbase.DBus
  ( Call
  , Help
  , Usage
  , Com(..)
  , Nameable(..)
  , withoutHelp
  , packName, unpackName
  , moonbaseBusName
  , moonbaseCliBusName
  , moonbaseInterfaceName
  , moonbaseObjectPath
  , withInterface
  , withObjectPath
  , withMemberName
  , connectDBus

  -- re-exports
  , ObjectPath
  , InterfaceName
  , MemberName
  , Variant
  , toVariant, fromVariant
  , Signal
  , signalBody
  , SignalHandler
  , IsValue(..)
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import           DBus
import           DBus.Client

import           Data.Char                   (isUpper, toLower, toUpper)
import qualified Data.Map                    as M
import           Data.Maybe

import           Moonbase.Core



type Call     = (ObjectPath, InterfaceName, MemberName)
type Help     = String
type Usage    = String

class (Moonbase rt m) => Com rt m where
  call     :: Call -> [Variant] -> MB rt m [Variant]
  call_    :: Call -> [Variant] -> MB rt m ()
  on       :: (Nameable a) => a -> Help -> Usage -> ([String] -> MB rt m String) -> MB rt m ()
  callback :: (Nameable a) => a -> ([String] -> MB rt m ()) -> MB rt m SignalHandler
  emit     :: String -> [String] -> MB rt m ()

class Nameable a where
  prepareName :: a -> (String, String)

instance Nameable String where
  prepareName n = (n, lower)
    where
      lower = map toLower n

instance Nameable (String, String) where
  prepareName (n, a) = (n, a)

withoutHelp :: String
withoutHelp = "No help is available."

packName :: String -> String
packName []       = []
packName (' ':xs) = packName xs
packName ('-':x:xs) = toUpper x : packName xs
packName ('_':x:xs) = toUpper x : packName xs
packName (x:xs)     = x : packName xs

unpackName :: String -> String
unpackName [] = []
unpackName (x:xs)
  | isUpper x = '-' : toLower x : unpackName xs
  | otherwise = x : unpackName xs

moonbaseBusName :: BusName
moonbaseBusName = "org.moonbase"

moonbaseCliBusName :: BusName
moonbaseCliBusName = "org.moonbase.cli"

moonbaseInterfaceName :: InterfaceName
moonbaseInterfaceName = "org.moonbase"

moonbaseObjectPath :: ObjectPath
moonbaseObjectPath = "/org/moonbase"

withInterface :: String -> InterfaceName
withInterface name = interfaceName_ $
    formatInterfaceName moonbaseInterfaceName ++ "." ++ name

withObjectPath :: String -> ObjectPath
withObjectPath name = objectPath_ $ formatObjectPath moonbaseObjectPath ++ "/" ++ name

withMemberName :: String -> MemberName
withMemberName = memberName_

connectDBus :: BusName -> IO (Maybe Client)
connectDBus bus = do
        client <- connectSession
        name   <- requestName client bus []
        return $ case name of
            NamePrimaryOwner -> Just client
            _                     -> Nothing
