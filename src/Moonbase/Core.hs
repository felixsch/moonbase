{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Moonbase.Core
    ( MoonState(..)
    , MoonConfig(..)
    , MoonError(..)
    , Moonbase(..)
    , runMoon, io
    , WindowManagerClass(..)
    , WindowManager(..)
    , StartStop(..), Enable(..)
    , Service(..)
    , Preferred(..)
    ) where

import System.IO
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import System.Environment.XDG.DesktopEntry

import Data.Monoid
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import qualified Data.Map as M

import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import DBus.Client


import Moonbase.Log
import Moonbase.Util.Trigger


data MoonState = MoonState
  { quit   :: Trigger
  , dbus   :: Client
  , wm     :: Maybe WindowManager
  , logHdl :: Handle
  , services :: M.Map String Service
  }


data MoonConfig = MoonConfig 
    { windowManager :: WindowManager 
    , autostart     :: [Service]
    , preferred      :: M.Map String Preferred
    }

data MoonError = ErrorMessage String
               | AppNotFound String
               | Quit

instance Error MoonError where
    noMsg  = ErrorMessage "Unknown Error"
    strMsg = ErrorMessage


newtype Moonbase a = Moonbase (ReaderT MoonConfig (StateT MoonState ( ErrorT MoonError IO)) a)
    deriving (Functor, Monad, MonadIO, MonadState MoonState, MonadReader MoonConfig, MonadError MoonError)

instance Applicative Moonbase where
    pure    = return
    (<*>)   = ap

instance (Monoid a) => Monoid (Moonbase a) where
    mempty = return mempty
    mappend = liftM2 mappend


instance Logger Moonbase where
    logM tag msg = do
        hdl <- logHdl <$> get
        date <- io $ formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
        io $ hPutStrLn hdl ("[" ++ date ++ "] " ++ show tag ++ ": " ++ msg) >> hFlush hdl

class WindowManagerClass a where
    startWM :: a -> Moonbase a
    stopWM ::  a -> Moonbase ()

instance WindowManagerClass WindowManager where
    startWM (WindowManager w) = WindowManager <$> startWM w
    stopWM (WindowManager w)  = stopWM w

data WindowManager = forall a. (WindowManagerClass a) => WindowManager a



class StartStop a where
    startService :: a -> Moonbase a
    stopService :: a -> Moonbase ()

    restartService :: a -> Moonbase a
    restartService = stopService >> startService

    isRunning :: a -> Moonbase Bool
    isRunning _ = return True

instance StartStop Service where
    startService (Service n a) = Service n <$> startService a
    stopService  (Service _ a) = stopService a

    restartService (Service n a) = Service n <$> restartService a


class Enable a where
    enableService :: a -> Moonbase ()

instance Enable Service where
    enableService (OneShot _ a) = enableService a
    
data Service = forall a. (StartStop a) => Service String a
             | forall a. (Enable a) =>    OneShot String a


data Preferred = Entry DesktopEntry
              | AppName String


 
runMoon :: MoonConfig -> MoonState -> Moonbase a -> IO (Either MoonError (a, MoonState))
runMoon
    conf st (Moonbase a) = runErrorT $ runStateT (runReaderT a conf) st


io :: (MonadIO m) => IO a -> m a
io
    = liftIO



