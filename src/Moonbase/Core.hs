{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Moonbase.Core
    ( Runtime(..)
    , Config(..)
    , RuntimeRef
    , MoonError(..)
    , Moonbase(..)
    , Name
    , runMoon
    , io
    , askRef
    , askConf

    , Ref
    , RefWrapper(..)
    , newRef
    , writeRef
    , readRef


    , Component(..)
    , ComponentM(..)
    , runComponentM
    , moon

    , Service(..)
    , Desktop(..)
    , Panel(..)
    , WindowManager(..)

    , HookType(..)
    , Hook(..)

    , Preferred(..)
    ) where

import System.IO
import System.Environment.XDG.DesktopEntry

import Data.Monoid
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Trans.Control

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar

import DBus.Client

import Moonbase.Log
import Moonbase.Util.Trigger


type Name = String

-- | Moonbase basic read write state
data Runtime = Runtime
  { quit        :: Trigger                   -- ^ MVar to triggere the quit event
  , dbus        :: Client                    -- ^ The core moonbase dbus session

  , logHdl      :: Handle                    -- ^ FileHandle to logfile
  , logVerbose  :: Bool                  -- ^ Log to stdout and file

  , stServices  :: M.Map Name RefWrapper    -- ^ All started services
  , stPanels    :: M.Map Name RefWrapper       -- ^ All running panels
  , stHooks     :: [Hook]                   -- ^ Enabled hooks
  , stWm        :: Maybe RefWrapper      -- ^ Windowmanager implementation is saved here
  , stDesktop   :: Maybe RefWrapper             -- ^ Desktop instance
  }

-- | Moonbase user configuration
-- Every user should create his own configuration as he needs
data Config = Config
    { wm :: WindowManager            -- ^ Windowmanager definition 
    , autostart     :: [Service]                -- ^ Many services which should be started
    , preferred      :: M.Map String Preferred  -- ^ A map of preffered applications for each mimetype
    , desk :: Desktop
    , panels :: [Panel]
    , hooks :: [Hook]
    }

-- | Error types for the ErrorT instance
data MoonError = ErrorMessage String  -- ^ generic Error
               | AppNotFound String   -- ^ Application which should be started was not found
               | FatalError String
               | InitFailed String
               | Quit                 -- ^ Triggeres quit (currently not used)
               deriving (Show)


type RuntimeRef = TVar Runtime
type MoonbaseT = ReaderT (Config, RuntimeRef) (ExceptT MoonError IO)

newtype Moonbase a = Moonbase { runMoonbase :: MoonbaseT a }
    deriving (Functor, Monad, MonadIO, MonadReader (Config, RuntimeRef), MonadError MoonError)

instance  Applicative Moonbase where
    pure  = return
    (<*>) = ap

instance MonadBase IO Moonbase where
    liftBase = Moonbase . lift . lift

instance MonadBaseControl IO Moonbase where
    newtype StM Moonbase a = StMoonbase { unStMoonbase :: StM MoonbaseT a }
    liftBaseWith f = Moonbase (liftBaseWith (\runIO -> f (fmap StMoonbase . runIO . runMoonbase)))
    restoreM = Moonbase . restoreM . unStMoonbase

askConf :: Moonbase Config
askConf = fst <$> ask

askRef :: Moonbase RuntimeRef
askRef = snd <$> ask

instance MonadState Runtime Moonbase where
    get = liftIO . readTVarIO =<< askRef
    put sta = do
        ref <- askRef
        liftIO $ atomically $ writeTVar ref sta

instance (Monoid a) => Monoid (Moonbase a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance Logger Moonbase where
    getLogName = return Nothing
    getVerbose = logVerbose <$> get
    getHdl = logHdl <$> get



 
runMoon :: Config -> RuntimeRef -> Moonbase a -> IO (Either MoonError a)
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

type Ref st = TVar st


newRef :: (MonadIO m) => st -> m (Ref st)
newRef = liftIO . atomically . newTVar

writeRef :: (MonadIO m) => Ref st -> st -> m ()
writeRef state' state'' = liftIO $ atomically $ writeTVar state' state''

readRef :: (MonadIO m) => Ref st -> m st
readRef = liftIO . readTVarIO


data RefWrapper = forall st. (Component st) => RefWrapper (Ref st)



newtype ComponentM st a = ComponentM (ReaderT (Name, Ref st) Moonbase a)
    deriving (Functor, Monad, MonadIO, MonadReader (Name, Ref st), MonadError MoonError)

instance Applicative (ComponentM st) where
    pure  = return
    (<*>) = ap

instance MonadBase Moonbase (ComponentM st) where
    liftBase = ComponentM . lift

instance MonadState st (ComponentM st) where
    get     = readRef =<< snd <$> ask
    put sta =  do
        ref <- snd <$> ask
        writeRef ref sta

instance (Component st) => Logger (ComponentM st) where
    getHdl = logHdl <$> liftBase get
    getVerbose = logVerbose <$> liftBase get
    getLogName = Just <$> componentName


runComponentM :: (Component st) => Name -> Ref st -> ComponentM st a -> Moonbase a
runComponentM name state' (ComponentM cmd) = runReaderT cmd (name, state')

componentName :: ComponentM st Name
componentName = fst <$> ask

class Component st where 

    start     :: ComponentM st Bool
    stop      :: ComponentM st ()

    restart   :: ComponentM st Bool
    restart   = stop >> start

    isRunning :: ComponentM st Bool


moon :: (Component st) => Moonbase a -> ComponentM st a
moon = ComponentM . lift

data Desktop = forall st. (Component st) => Desktop
  { desktopName :: Name
  , desktopRequires :: [Hook]
  , desktop     :: st }

data Service = forall st. (Component st) => Service
  { serviceName :: Name
  , serviceRequires :: [Hook]
  , service :: st }

data Panel = forall st. (Component st) => Panel
  { panelName :: Name
  , panelRequires :: [Hook]
  , panel :: st }

data WindowManager = forall st. (Component st) => WindowManager
 { windowManagerName :: Name
 , windowManagerRequires :: [Hook]
 , windowManager :: st }

-- Preferred ------------------------------------------------------------------
data Preferred = Entry DesktopEntry
              | AppName String



