{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Moonbase.Core
    ( MoonState(..)
    , MoonConfig(..)
    , MoonRuntime
    , MoonError(..)
    , Moonbase(..)
    , Name
    , runMoon, io
    , WindowManager(..)
    , StartStop(..), Enable(..), Requires(..)
    , Service(..)
    , Preferred(..)
    , Panel(..)
    , HookType(..)
    , Hook(..)
    , Desktop(..)
    , askRef
    , askConf
    ) where

import System.IO
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import System.Environment.XDG.DesktopEntry

import Data.Monoid
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import qualified Data.Map as M

import Control.Applicative

import Data.IORef
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import DBus.Client


import Moonbase.Log
import Moonbase.Util.Trigger


type Name = String

-- | Moonbase basic read write state
data MoonState = MoonState
  { quit   :: Trigger                   -- ^ MVar to triggere the quit event
  , dbus   :: Client                    -- ^ The core moonbase dbus session
  , wm     :: Maybe WindowManager       -- ^ Windowmanager implementation is saved here
  , desk   :: Maybe Desktop             -- ^ Desktop instance
  , logHdl :: Handle                    -- ^ FileHandle to logfile
  , logVerbose :: Bool                  -- ^ Log to stdout and file
  , services :: M.Map Name Service    -- ^ All started services
  , pnls    :: M.Map Name Panel       -- ^ All running panels
  , hks     :: [Hook]                   -- ^ Enabled hooks
  }

-- | Moonbase user configuration
-- Every user should create his own configuration as he needs
data MoonConfig = MoonConfig 
    { windowManager :: WindowManager            -- ^ Windowmanager definition 
    , autostart     :: [Service]                -- ^ Many services which should be started
    , preferred      :: M.Map String Preferred  -- ^ A map of preffered applications for each mimetype
    , desktop :: Desktop
    , panels :: [Panel]
    , hooks :: [Hook]
    }

-- | Error types for the ErrorT instance
data MoonError = ErrorMessage String  -- ^ generic Error
               | AppNotFound String   -- ^ Application which should be started was not found
               | Quit                 -- ^ Triggeres quit (currently not used)


instance Error MoonError where
    noMsg  = ErrorMessage "Unknown Error"
    strMsg = ErrorMessage

type MoonRuntime = IORef MoonState

newtype Moonbase a = Moonbase (ReaderT (MoonConfig, MoonRuntime) (ErrorT MoonError IO) a)
    deriving (Functor, Monad, MonadIO, MonadReader (MoonConfig, MoonRuntime), MonadError MoonError)

instance Applicative Moonbase where
    pure    = return
    (<*>)   = ap


instance MonadState MoonState Moonbase where
    get = io . readIORef =<< askRef
    put v = do
             r <- askRef
             io $ writeIORef r v


askConf :: Moonbase MoonConfig
askConf
    = fst <$> ask

askRef :: Moonbase MoonRuntime
askRef
    = snd <$> ask

instance (Monoid a) => Monoid (Moonbase a) where
    mempty = return mempty
    mappend = liftM2 mappend


instance Logger Moonbase where
    logM tag msg = do
        hdl <- logHdl <$> get
        verbose <- logVerbose <$> get
        date <- io $ formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
        io $ hPutStrLn hdl ("[" ++ date ++ "] " ++ show tag ++ ": " ++ msg) >> hFlush hdl
        when verbose (io $ putStrLn ("[" ++ date ++ "] " ++ show tag ++ ": " ++ msg))

    debugM msg = do
        date <- io $ formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
        hdl <- logHdl <$> get
        verbose <- logVerbose <$> get
        when verbose (io $ hPutStrLn hdl ("[" ++ date ++ "]        : " ++ msg) >> hFlush hdl)
        when verbose (io $ putStrLn ("[" ++ date ++ "]        : " ++ msg))


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

instance StartStop Panel where
    start (Panel n a) = Panel n <$> start a
    stop  (Panel _ a) = stop a

    isRunning (Panel _ a) = isRunning a


class Enable a where
    enable :: a -> Moonbase ()

instance Enable Service where
    enable (OneShot _ a) = enable a

instance Enable Hook where
    enable (Hook _ _ a) = enable a

data HookType = HookStart
                  | HookAfterStartup
                  | HookBeforeQuit
                  | HookQuit
                  deriving(Show, Eq)

data Hook = forall a. (Enable a) => Hook Name HookType a

instance Eq Hook where
    (Hook aName aType _) == (Hook bName bType _)  = aName == bName && aType == bType

class Requires a where
    requires :: a -> [Hook]
    requires _ = []

instance Requires Service where
    requires (Service _ a) = requires a
    requires (OneShot _ a) = requires a

instance Requires Desktop where
    requires (Desktop _ a) = requires a

instance Requires WindowManager where
    requires (WindowManager _ a) = requires a

instance Requires Panel where
    requires (Panel _ a) = requires a
     
data Service = forall a. (Requires a, StartStop a) => Service Name a
             | forall a. (Requires a, Enable a) =>    OneShot Name a

data Desktop = forall a. (Requires a, StartStop a) => Desktop Name a

data WindowManager = forall a. (Requires a, StartStop a) => WindowManager Name a

data Panel = forall a. (Requires a, StartStop a) => Panel Name a


data Preferred = Entry DesktopEntry
              | AppName String
 
runMoon :: MoonConfig -> MoonRuntime -> Moonbase a -> IO (Either MoonError a)
runMoon
    conf st (Moonbase a) = runErrorT $ runReaderT a (conf, st)


io :: (MonadIO m) => IO a -> m a
io
    = liftIO



