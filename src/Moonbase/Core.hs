{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Moonbase.Core
    ( MoonState(..)
    , MoonConfig(..)
    , MoonError(..)
    , Moonbase(..)
    , runMoon, io
    , WindowManagerClass(..)
    , WindowManager(..)
    ) where

import System.IO
import System.Locale (defaultTimeLocale, rfc822DateFormat)

import Data.Monoid
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)

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
  }


data MoonConfig = MoonConfig 
    { windowManager :: WindowManager 
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

 
runMoon :: MoonConfig -> MoonState -> Moonbase a -> IO (Either MoonError (a, MoonState))
runMoon
    conf st (Moonbase a) = runErrorT $ runStateT (runReaderT a conf) st


io :: (MonadIO m) => IO a -> m a
io
    = liftIO



