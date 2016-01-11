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
 , MB(..)
 , moon
 , eval
 , Message(..)
 , DBusClient
 , ActionType(..)
 , Action(..)
 , actionName, actionType, actionHelp, action

 -- re-imports
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
  log       :: (Moon m) => Message -> MB rt m ()
  dbus      :: (Moon m) => MB rt m DBusClient
  theme     :: (Moon m) => MB rt m Theme
  verbose   :: (Moon m) => MB rt m Bool
  add       :: (Moon m) => String -> Action m rt -> MB rt m ()
  actions   :: (Moon m) => MB rt m (M.Map String (Action m rt))

-- Actions ---------------------------------------------------------------------

data ActionType = ActionCommand
                | ActionFunction
                | ActionRaw

data Action rt m = Action
  { _actionName :: String
  , _actionHelp :: String
  , _actionType :: ActionType
  , _action     :: [String] -> MB rt m String }

-- command "toggle" `on` "top-panel" `with` do

-- Moonbase --------------------------------------------------------------------

class (Moon m, Base rt) => Moonbase rt m


newtype (Moonbase rt m) => MB rt m a = MB (ReaderT (BaseRef rt) m a)
  deriving (Functor, Monad, MonadReader (BaseRef rt))

moon :: (Moonbase rt m) => m a -> MB rt m a
moon = MB . lift

instance (Monad m) => Applicative (MB rt m) where
  pure  = return
  (<*>) = ap


instance (Moonbase rt m) => Moon (MB rt m) where
  io      = moon . io
  puts    = moon . puts
  content = moon . content
  fork f  = do
    ref <- ask
    moon $ fork (void $ eval ref f)
  timeout s f = do
    ref <- ask
    moon $ timeout s (eval ref f)


eval :: (Moonbase rt m) => BaseRef rt -> MB rt m a -> m a
eval ref (MB f) = runReaderT f ref


-- Runtime ---------------------------------------------------------------------
-- TODO: Move me to Moonbase.hs
--data Runtime m = Runtime
-- { _hdl       :: Handle
-- , _actions   :: [Action (Runtime m) m]
-- , _theme     :: Theme
-- , _isVerbose :: Bool
-- , _dbus      :: DBusClient }
-- makeLenses ''Runtime

makeLenses ''Action
