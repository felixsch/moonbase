{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Moonbase.DBus
  ( Call
  , Signal
  , Help
  , Com(..)
  , Nameable(..)
  , withoutHelp
  , sanatizeName
  , moonbaseBusName
  , moonbaseCliBusName
  , moonbaseInterfaceName
  , moonbaseObjectPath
  , withInterface
  , withObjectPath
  , connectDBus

  -- re-expors
  , DBus.ObjectPath
  , DBus.InterfaceName
  , DBus.MemberName
  , DBus.Variant

  ) where

import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import qualified DBus
import qualified DBus.Client                 as DBus

import           Data.Char                   (toLower, toUpper)
import qualified Data.Map                    as M
import           Data.Maybe

import           Moonbase.Core



type Call     = (DBus.ObjectPath, DBus.InterfaceName, DBus.MemberName)
type Signal   = String
type Help     = String

class (Moonbase rt m) => Com rt m where
  call     :: Call -> [DBus.Variant] -> MB rt m [DBus.Variant]
  call_    :: Call -> [DBus.Variant] -> MB rt m ()
  on       :: (Nameable a) => a -> Help -> ([String] -> MB rt m String) -> MB rt m ()
  callback :: (Signal -> MB rt m ()) -> MB rt m ()

class Nameable a where
  prepareName :: a -> (String, String)

instance Nameable String where
  prepareName n = (sanatizeName n, lower)
    where
      lower = map toLower n

instance Nameable (String, String) where
  prepareName (n, a) = (sanatizeName n, a)

withoutHelp :: String
withoutHelp = "No help is available."

sanatizeName :: String -> String
sanatizeName []       = []
sanatizeName (' ':xs) = sanatizeName xs
sanatizeName ('-':x:xs) = toUpper x : sanatizeName xs
sanatizeName ('_':x:xs) = toUpper x : sanatizeName xs
sanatizeName (x:xs)     = x : sanatizeName xs


moonbaseBusName :: DBus.BusName
moonbaseBusName = "org.moonbase"

moonbaseCliBusName :: DBus.BusName
moonbaseCliBusName = "org.moonbase.cli"

moonbaseInterfaceName :: DBus.InterfaceName
moonbaseInterfaceName = "org.moonbase"

moonbaseObjectPath :: DBus.ObjectPath
moonbaseObjectPath = "/org/moonbase"

withInterface :: String -> DBus.InterfaceName
withInterface name = DBus.interfaceName_ $
    DBus.formatInterfaceName moonbaseInterfaceName ++ "." ++ name

withObjectPath :: String -> DBus.ObjectPath
withObjectPath name = DBus.objectPath_ $ DBus.formatObjectPath moonbaseObjectPath ++ "/" ++ name

connectDBus :: DBus.BusName -> IO (Maybe DBus.Client)
connectDBus bus = do
        client <- DBus.connectSession
        name   <- DBus.requestName client bus []
        return $ case name of
            DBus.NamePrimaryOwner -> Just client
            _                     -> Nothing
