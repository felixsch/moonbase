{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Moonbase.Core
    ( MoonState(..)
    , MoonConfig(..)
    , MoonError(..)
    , Moonbase(..)
    , runMoon, io
    , WindowManager(..)
    , StartStop(..), Enable(..)
    , Service(..)
    , Preferred(..)
    , Desktop(..)
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

-- | Moonbase basic read write state
data MoonState = MoonState
  { quit   :: Trigger                   -- ^ MVar to triggere the quit event
  , dbus   :: Client                    -- ^ The core moonbase dbus session
  , wm     :: Maybe WindowManager       -- ^ Windowmanager implementation is saved here
  , desk   :: Maybe Desktop
  , logHdl :: Handle                    -- ^ FileHandle to logfile
  , services :: M.Map String Service    -- ^ All started services
  }

-- | Moonbase user configuration
-- Every user should create his own configuration as he needs
data MoonConfig = MoonConfig 
    { windowManager :: WindowManager            -- ^ Windowmanager definition 
    , autostart     :: [Service]                -- ^ Many services which should be started
    , preferred      :: M.Map String Preferred  -- ^ A map of preffered applications for each mimetype
    , desktop :: Desktop
    }

-- | Error types for the ErrorT instance
data MoonError = ErrorMessage String  -- ^ generic Error
               | AppNotFound String   -- ^ Application which should be started was not found
               | Quit                 -- ^ Triggeres quit (currently not used)


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

class StartStop a where
    start :: a -> Moonbase a
    stop :: a -> Moonbase ()

    restart :: a -> Moonbase a
    restart = stop >> start

    isRunning :: a -> Moonbase Bool
    isRunning _ = return True



instance StartStop Service where
    start (Service n a) = Service n <$> start a
    stop  (Service _ a) = stop a

    isRunning (Service _ a) = isRunning a


instance StartStop Desktop where
    start (Desktop n a) = Desktop n <$> start a
    stop  (Desktop _ a) = stop a

    isRunning (Desktop _ a) = isRunning a

instance StartStop WindowManager where
    start (WindowManager n a) = WindowManager n <$> start a
    stop  (WindowManager _ a) = stop a

    isRunning (WindowManager _ a) = isRunning a


class Enable a where
    enable :: a -> Moonbase ()

instance Enable Service where
    enable (OneShot _ a) = enable a
    
data Service = forall a. (StartStop a) => Service String a
             | forall a. (Enable a) =>    OneShot String a


data Desktop = forall a. (StartStop a) => Desktop String a

data WindowManager = forall a. (StartStop a) => WindowManager String a


data Preferred = Entry DesktopEntry
              | AppName String
 
runMoon :: MoonConfig -> MoonState -> Moonbase a -> IO (Either MoonError (a, MoonState))
runMoon
    conf st (Moonbase a) = runErrorT $ runStateT (runReaderT a conf) st


io :: (MonadIO m) => IO a -> m a
io
    = liftIO



