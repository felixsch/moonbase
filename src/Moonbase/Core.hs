{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Moonbase.Core
    ( MoonState(..)
    , MoonConfig(..)
    , MoonRuntime
    , MoonError(..)
    , Moonbase(..)
    , Name
    , runMoon, io, moon
    , WindowManager(..)
    , StartStop(..), Requires(..)
    , Preferred(..)
    , Panel(..)
    , HookType(..)
    , Hook(..)
    , Desktop(..)
    , askRef
    , askConf
    --
    , IsService(..)
    , ServiceError(..)
    , Service(..)
    , runServiceT
    ) where

import System.IO
import System.Environment.XDG.DesktopEntry

import Data.Monoid
import qualified Data.Map as M

import Control.Applicative

import Data.IORef
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
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
               | FatalError String
               | Quit                 -- ^ Triggeres quit (currently not used)
               deriving (Show)



type MoonRuntime = IORef MoonState

newtype Moonbase a = Moonbase (ReaderT (MoonConfig, MoonRuntime) (ExceptT MoonError IO) a)
    deriving (Functor, Monad, MonadIO, MonadReader (MoonConfig, MoonRuntime), MonadError MoonError)

class (MonadIO m, Logger m, Applicative m) => MonadMB m where
    moon :: Moonbase a -> m a

instance MonadMB Moonbase where
    moon = id

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
        message <- formatMessage tag Nothing msg
        io $ hPutStrLn hdl message >> hFlush hdl
        when verbose $ io $ putStrLn message

    debugM msg = do
        hdl <- logHdl <$> get
        verbose <- logVerbose <$> get
        message <- formatMessage DebugTag Nothing msg
        when verbose $ io $ hPutStrLn hdl message >> hFlush hdl
        when verbose $ io $ putStrLn message


class StartStop a where
    start :: a -> Moonbase a
    stop :: a -> Moonbase ()

    restart :: a -> Moonbase a
    restart = stop >> start

    isRunning :: a -> Moonbase Bool
    isRunning _ = return True

-- Services

data ServiceError = ServiceError String
                  | ServiceWarning  String
                  | ServiceFatalError String
                  deriving (Show)



newtype ServiceT st a = ServiceT (StateT st (ExceptT ServiceError Moonbase) a)
    deriving (Functor, Monad, MonadIO, MonadState st, MonadError ServiceError)


instance MonadMB (ServiceT st) where
    moon = ServiceT . lift . lift

  
instance (Monoid a) => Monoid (ServiceT st a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance Applicative (ServiceT st) where
    pure = return
    (<*>) = ap

instance Logger (ServiceT st) where
    logM tag msg = do
        hdl <- logHdl <$> moon get
        verbose <- logVerbose <$> moon get
        message <- formatMessage tag (Just "Service") msg
        io $ hPutStrLn hdl message >> hFlush hdl
        when verbose $ io $ putStrLn message

    debugM msg = do
        hdl <- logHdl <$> moon get
        verbose <- logVerbose <$> moon get
        message <- formatMessage DebugTag (Just "Service") msg
        io $ hPutStrLn hdl message >> hFlush hdl
        when verbose $ io $ putStrLn message

class IsService st where

    initState :: ServiceT st st

    startService :: ServiceT st Bool
    stopService  :: ServiceT st ()
    
    restartService :: ServiceT st Bool 
    restartService = stopService >> startService

    isServiceRunning :: ServiceT st Bool


data Service = forall st. (Requires st, IsService st) => Service Name st

runServiceT :: ServiceT st a -> st -> Moonbase (Either ServiceError (a, st))
runServiceT (ServiceT cmd) = runExceptT . runStateT cmd


-- Service End 

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

data HookType = HookStart
                  | HookAfterStartup
                  | HookBeforeQuit
                  | HookQuit
                  deriving(Show, Eq)


data Hook = Hook Name HookType (Moonbase ())

instance Eq Hook where
    (Hook aName aType _) == (Hook bName bType _)  = aName == bName && aType == bType

class Requires a where
    requires :: a -> [Hook]
    requires _ = []

instance Requires Service where
    requires (Service _ a) = requires a

instance Requires Desktop where
    requires (Desktop _ a) = requires a

instance Requires WindowManager where
    requires (WindowManager _ a) = requires a

instance Requires Panel where
    requires (Panel _ a) = requires a



data Desktop = forall a. (Requires a, StartStop a) => Desktop Name a

data WindowManager = forall a. (Requires a, StartStop a) => WindowManager Name a

data Panel = forall a. (Requires a, StartStop a) => Panel Name a


data Preferred = Entry DesktopEntry
              | AppName String
 
runMoon :: MoonConfig -> MoonRuntime -> Moonbase a -> IO (Either MoonError a)
runMoon
    conf st (Moonbase a) = runExceptT $ runReaderT a (conf, st)

io :: (MonadIO m) => IO a -> m a
io
    = liftIO



