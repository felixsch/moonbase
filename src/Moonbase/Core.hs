{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Moonbase.Core
    ( MoonState(..)
    , MoonConfig(..)
    , MoonRuntime
    , MoonError(..)
    , Moonbase(..)
    , Name
    , runMoon
    , io
    , moon
    , askRef
    , askConf

    , StartStop(..)
    , Requires(..)

    , Preferred(..)
    --
    , HookType(..)
    , Hook(..)
    --
    , ServiceT(..)
    , Service(..)
    , ServiceError(..)
    , runServiceT
    --
    , DesktopT(..)
    , Desktop(..)
    , DesktopError(..)
    , runDesktopT
    --
    , PanelError(..)
    , Panel(..)
    , PanelT(..)
    , runPanelT
    --
    , WindowManagerT(..)
    , WindowManager(..)
    , WMError(..)
    , runWindowManagerT
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

class (Monad m, MonadIO m, Logger m) => MonadMB m where
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


instance (Monoid a) => Monoid (Moonbase a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance Logger Moonbase where
    getLogName = return Nothing
    getVerbose = logVerbose <$> get
    getHdl = logHdl <$> get


askConf :: Moonbase MoonConfig
askConf
    = fst <$> ask

askRef :: Moonbase MoonRuntime
askRef
    = snd <$> ask

 
runMoon :: MoonConfig -> MoonRuntime -> Moonbase a -> IO (Either MoonError a)
runMoon
    conf st (Moonbase a) = runExceptT $ runReaderT a (conf, st)

io :: (MonadIO m) => IO a -> m a
io
    = liftIO

-- Hook -----------------------------------------------------------------------

data HookType = HookStart
                  | HookAfterStartup
                  | HookBeforeQuit
                  | HookQuit
                  deriving(Show, Eq)


data Hook = Hook Name HookType (Moonbase ())

instance Eq Hook where
    (Hook aName aType _) == (Hook bName bType _)  = aName == bName && aType == bType

-- Basic classes --------------------------------------------------------------

class Requires a where
    requires :: a -> [Hook]
    requires _ = []


class (MonadMB (m st)) => StartStop st m where
    start :: m st Bool
    stop :: m st ()

    restart :: m st Bool
    restart = stop >> start

    isRunning :: m st Bool


-- Desktop --------------------------------------------------------------------
data DesktopError = DesktopError String

newtype DesktopT st a = DesktopT (StateT st (ExceptT DesktopError Moonbase) a)
    deriving (Functor, Monad, MonadIO, MonadState st, MonadError DesktopError)

runDesktopT ::  DesktopT st a -> st -> Moonbase (Either DesktopError (a, st))
runDesktopT (DesktopT cmd) = runExceptT . runStateT cmd

instance Applicative (DesktopT st) where
    pure = return
    (<*>) = ap

instance MonadMB (DesktopT st) where
    moon = DesktopT . lift . lift

instance Logger (DesktopT st) where
    getHdl = logHdl <$> moon get
    getVerbose = logVerbose <$> moon get
    getLogName = return $ Just "Desktop"

instance Requires Desktop where
    requires (Desktop _ st) = requires st


data Desktop = forall st. (Requires st, StartStop st DesktopT) => Desktop Name st



-- Service --------------------------------------------------------------------

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
    getLogName = return $ Just "Service"
    getVerbose = logVerbose <$> moon get
    getHdl     = logHdl <$> moon get

data Service = forall st. (Requires st, (StartStop st ServiceT)) => Service Name st

instance Requires Service where
    requires (Service _ st) = requires st

runServiceT :: ServiceT st a -> st -> Moonbase (Either ServiceError (a, st))
runServiceT (ServiceT cmd) = runExceptT . runStateT cmd


-- WindowManager --------------------------------------------------------------

data WMError = WMError String
    deriving (Show)

newtype WindowManagerT st a = WindowManagerT (StateT st (ExceptT WMError Moonbase) a)
    deriving (Functor, Monad, MonadIO, MonadState st, MonadError WMError)

instance MonadMB (WindowManagerT st) where
    moon = WindowManagerT . lift . lift

instance (Monoid a) => Monoid (WindowManagerT st a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance Applicative (WindowManagerT st) where
    pure = return
    (<*>) = ap

instance Logger (WindowManagerT st) where
    getLogName = return $ Just "WindowManager"
    getVerbose = logVerbose <$> moon get
    getHdl     = logHdl <$> moon get

data WindowManager = forall st. (Requires st, StartStop st WindowManagerT) => WindowManager Name st

instance Requires WindowManager where
    requires (WindowManager _ st) = requires st

runWindowManagerT :: WindowManagerT st a -> st -> Moonbase (Either WMError (a, st))
runWindowManagerT (WindowManagerT cmd) = runExceptT . runStateT cmd

-- Panel ----------------------------------------------------------------------

data PanelError = PanelError String

newtype PanelT st a = PanelT (StateT st (ExceptT PanelError Moonbase) a)
    deriving (Functor, Monad, MonadIO, MonadState st, MonadError PanelError)


instance MonadMB (PanelT st) where
    moon = PanelT . lift . lift

instance (Monoid a) => Monoid (PanelT st a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance Applicative (PanelT st) where
    pure = return
    (<*>) = ap

instance Logger (PanelT st) where
    getLogName = return $ Just "Panel"
    getVerbose = logVerbose <$> moon get
    getHdl     = logHdl <$> moon get

data Panel = forall st. (Requires st, StartStop st PanelT) => Panel Name st

instance Requires Panel where
    requires (Panel _ st) = requires st

runPanelT :: PanelT st a -> st -> Moonbase (Either PanelError (a,st))
runPanelT (PanelT cmd) = runExceptT . runStateT cmd 

-- Preferred ------------------------------------------------------------------
data Preferred = Entry DesktopEntry
              | AppName String



