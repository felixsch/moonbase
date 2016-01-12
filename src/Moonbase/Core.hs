{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Moonbase.Core
 ( Exception(..)
 , Moon(..)
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
import           Prelude              hiding (log)
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

instance Show Exception where
  show CouldNotOpenDisplay = "Could not open display"
  show (DBusError err)     = "DBus error: " ++ err
  show (FileNotFound path) = "No such file or directory: " ++ path
  show Shutdown            = "Bye!"


--instance E.Exception Exception

data Message = Warning String   -- ^ A warning
             | Info String      -- ^ An information
             | Success String   -- ^ Successfully exectued action
             | Debug String     -- ^ Debuging output

instance Show Message where
  show (Warning str) = "Warning: " ++ str
  show (Info    str) = "Info: " ++ str
  show (Success str) = "Success: " ++ str
  show (Debug   str) = "Debug: " ++ str


class Monad m => Moon m where
  io      :: IO a -> m a
  puts    :: String -> m ()
  content :: FilePath -> m String
  fork    :: m () -> m ThreadId
  delay   :: Int -> m ()
  timeout :: Int -> m a  -> m (Maybe a)

class (Moon m) => Moonbase rt m where
  data Base rt :: *
  log       :: Message -> MB rt m ()
  theme     :: MB rt m Theme
  verbose   :: MB rt m Bool
  add       :: String -> Action rt m -> MB rt m ()
  actions   :: MB rt m (M.Map String (Action rt m))

-- Actions ---------------------------------------------------------------------

data ActionType = ActionCommand
                | ActionFunction
                | ActionRaw
                deriving (Eq, Show)

data Action rt m = Action
  { _actionName :: String
  , _actionHelp :: String
  , _actionType :: ActionType
  , _action     :: [String] -> MB rt m String }

-- command "toggle" `on` "top-panel" `with` do

-- Moonbase --------------------------------------------------------------------

newtype (Moonbase rt m) => MB rt m a = MB (ReaderT (Base rt) m a)
  deriving (Functor, Monad, MonadReader (Base rt))

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
  delay   = moon . delay
  timeout s f = do
    ref <- ask
    moon $ timeout s (eval ref f)


eval :: (Moonbase rt m) => Base rt -> MB rt m a -> m a
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
