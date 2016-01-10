{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Moonbase.Core
 ( Exception(..)
 , Moon(..)
 , Base(..)
 , Moonbase(..)
 , moon
 , eval
 , Message(..)

 , DBusClient
 , Argument

 , Action(..)
 , actionName, actionHelp, action
 , ActionError(..)
 , ActionResult(..)
 , actionResult
 , actionError
 , actionNothing

 , Runtime(..)
 , hdl, actions, theme, dbus, isVerbose

 -- re-imports
 , get, put, modify
 , ask
 , E.throw
 ) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception    as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           System.IO

import qualified Data.Map             as M
import           Data.Monoid

import qualified DBus.Client          as DBus

import           Moonbase.Theme


-- Core Types ------------------------------------------------------------------

type DBusClient = DBus.Client

data Exception = CouldNotOpenDisplay
               | DBusError String
               | FileNotFound FilePath
               | Shutdown
               deriving (Show)

instance E.Exception Exception

data Message = Warning    -- ^ A warning
             | Info       -- ^ An information
             | Success    -- ^ Successfully exectued action
             | Debug      -- ^ Debuging output
             deriving (Show, Eq, Enum)


class Monad m => Moon m where
  io      :: IO a -> m a
  puts    :: String -> m ()
  content :: FilePath -> m String
  fork    :: m () -> m ThreadId
  timeout :: Int -> m a  -> m (Maybe a)

class Base rt where
  data BaseRef rt :: *
  newBase :: (Moon m) => rt -> m (BaseRef rt)
  base :: (Moon m) => BaseRef rt -> m rt
  update :: (Moon m) => BaseRef rt -> rt -> m ()

  logB  :: (Moon m) => BaseRef rt -> Message -> m ()
  dbusB :: (Moon m) => BaseRef rt -> m DBusClient

  addActionB :: (Moon m) => BaseRef rt -> String -> Action rt m -> Moonbase st m ()
  allActionsB :: (Moon m) => BaseRef rt -> Moonbase st m [Action rt m]



-- Actions ---------------------------------------------------------------------

type Argument = String
data ActionError = ActionNotFound
                  | ActionFileNotFound
                  | ActionError String
                  deriving (Show)

type ActionResult = Either ActionError (Maybe String)

actionError :: ActionError -> ActionResult
actionError = Left

actionResult :: (Show a) => a -> ActionResult
actionResult = Right . Just . show

actionNothing :: ActionResult
actionNothing = Right Nothing

data Action rt m = Action
  { _actionName :: String
  , _actionHelp :: String
  , _action     :: [Argument] -> Moonbase rt m ActionResult }


-- Runtime ---------------------------------------------------------------------
-- TODO: Move me to Moonbase.hs
data Runtime m = Runtime
 { _hdl       :: Handle
 , _actions   :: [Action (Runtime m) m]
 , _theme     :: Theme
 , _isVerbose :: Bool
 , _dbus      :: DBusClient }


-- Moonbase --------------------------------------------------------------------

newtype (Base rt) => Moonbase rt m a = Moonbase (ReaderT (BaseRef rt) m a)
  deriving (Functor, Monad, MonadReader (BaseRef rt))

moon :: (Moon m, Base rt) => m a -> Moonbase rt m a
moon = Moonbase . lift

instance (Monad m) => Applicative (Moonbase rt m) where
  pure  = return
  (<*>) = ap

instance (Moon m, Base rt) => MonadState rt (Moonbase rt m) where
  get = base =<< ask
  put rt = do
    ref <- ask
    update ref rt

instance (Base rt, Moon m) => Moon (Moonbase rt m) where
  io      = moon . io
  puts    = moon . puts
  content = moon . content
  fork f  = do
    ref <- ask
    moon $ fork (void $ eval ref f)
  timeout s f = do
    ref <- ask
    moon $ timeout s (eval ref f)


eval :: (Base rt, Moon m) => BaseRef rt -> Moonbase rt m a -> m a
eval ref (Moonbase f) = runReaderT f ref

makeLenses ''Action
makeLenses ''Runtime
