{-|
Module      : Moonbase
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Moonbase 
  ( MSignal(..)
  , isFatal

  , Name
  , Theme(..)
  , Config(..)
  , Runtime(..)
  , Moonbase(..)
  , io
  , getTheme
  , getConfig
  , moonbase
  , push
  , addHook
  , addCleanup
  , withComponent
  , getComponent
  , modifyComponent
  
  , HookType(..)
  , Hook(..)
  , makeHook

  , ComponentM(..)
  , Component
  , newComponent
  , newComponentWithCleanup
  , moon
  , setTheme
  , withHook
  , warn
  , info
  , initFailed

  , moonBusName
  , moonInterface
  , addDBusMethod
  , wrap0
  , wrap1
  , wrap2
  , wrap3
  , addDBusSignal

  , Preferred(..)
  ) where
  
import System.IO
import System.Directory
import System.FilePath.Posix
import Data.Time.Format
import Data.Time.LocalTime
import System.Environment
import System.Environment.XDG.DesktopEntry
import System.Environment.XDG.BaseDir

import Control.Monad
import Control.Monad.Base
import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent

import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Sequence as S


import Moonbase.Theme
import Moonbase.Util.Application


import DBus.Client
import DBus


-- | Signal definitions which are used to indicate moonbase state
--
-- Signals are threadsafe an can be pushed to the signal queue from
-- every thread
--
data MSignal = Shutdown                   
             -- ^ Initiates a shutdown request

             | Exit                       
             -- ^ Gracefully stop moonbase 
             -- (Do not use this unless you know what you are doing)

             | ApplicationNotFound String
             -- ^ Indicates that an application was not found.
             -- (Does not force stop)

             | FatalError String
             -- ^ Raise a fatal error and stop gracefully

             | Warning String
             -- ^ Show a warning

             | Info String
             -- ^ Show a information

             | Message String
             -- ^ Show a plain message

             | InitFailed Bool String
             -- ^ Indicate that a component failed to start. If first prameter is
             -- set to True this signal is considered as fatal and will stop
             -- gracefully


instance Show MSignal where
    show Shutdown                   = "Shutdown..."
    show Exit                       = "Bye bye!"
    show (Message msg)              = msg
    show (ApplicationNotFound app)  = "Could not find " ++ app ++ ". Aborting!"
    show (FatalError err)           = "Fatal error: " ++ err ++ " Aborting!"
    show (Warning msg)              = "Warning: " ++ msg
    show (Info msg)                 = "Info: " ++ msg
    show (InitFailed forceQuit err) = "Initialization failed: " ++ err ++ forced 
        where
            forced = if forceQuit
                        then " Aborting!"
                        else ""

-- | Checks if a signal is fatal
isFatal :: MSignal -> Bool
isFatal Shutdown                 = False
isFatal (Message _)              = False
isFatal (Warning _)              = False
isFatal (Info _)                 = False
isFatal (InitFailed forceQuit _) = forceQuit
isFatal _                        = True

-- | Checks if given signal is 'Shutdown'
isShutdown :: MSignal -> Bool 
isShutdown Shutdown = True
isShutdown _        = False


-- | Component Indentifier
type Name = String

-- | Extended configuration
data Config  = Config
  { terminal :: String      -- ^ Name of the terminal which should used everywhere
  }

-- | The state of moonbase which is used to transact data between components and
-- threads.
--
-- 'Runtime' is used via a 'TVar' to gain thread safety
data Runtime = Runtime 
    { dbus        :: Client                  -- ^ DBus client connection
    , logHandle   :: Handle                  -- ^ Log file handle
    , theme       :: Theme                   -- ^ moonbase theme
    , config      :: Config                  -- ^ Used configuration
    , signals     :: TQueue MSignal          -- ^ The signal queue
    , comps       :: M.Map Name Component    -- ^ Components listed by there name
    , preferred   :: Maybe Preferred         -- ^ All preferred applications by mime
    , cleanup     :: [Moonbase ()]           -- ^ Cleanup methods which are called before exit.
    , hooks       :: [Hook]                  -- ^ Hooks which are injected at some points
    }

-- | Generates a new emtpy runtime
newRuntime :: Client -> Handle -> Config -> TQueue MSignal -> Runtime
newRuntime client hdl conf sigs = Runtime
  { dbus        = client
  , logHandle   = hdl
  , theme       = defaultTheme
  , config      = conf
  , signals     = sigs 
  , comps       = M.empty
  , preferred   = Nothing
  , cleanup     = []
  , hooks       = []
  }


-- | Moonbase monad
-- 
-- Moonbase is a 'ReaderT' transformer chained to 'IO'. It handles 'TVar Runtime'
-- to get a mutable state.
-- 
-- 'Moonbase' is a instance of 'MonadState' which encapsules the 'TVar' variable
-- extracting and putting.
--
newtype Moonbase a = Moonbase { runMoonbase :: ReaderT (TVar Runtime) IO a }
    deriving (Functor, Monad, MonadIO, MonadReader (TVar Runtime))

instance MonadBase IO Moonbase where
    liftBase = Moonbase . lift

instance MonadBaseControl IO Moonbase where
    newtype StM Moonbase a = StMoonbase { unStMoonbase :: StM (ReaderT (TVar Runtime) IO) a }
    liftBaseWith f = Moonbase (liftBaseWith (\runIO -> f (fmap StMoonbase . runIO . runMoonbase)))
    restoreM = Moonbase . restoreM . unStMoonbase

instance Applicative Moonbase where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (Moonbase a) where
    mempty = return mempty
    mappend = liftM2 mappend

-- to make it easy to access runtime informations Moonbase is a instance of 
-- MonadState.
instance MonadState Runtime Moonbase where
    get = do
        ref <- ask
        liftIO $ readTVarIO ref
    put sta = do
        ref <- ask
        liftIO $ atomically $ writeTVar ref sta

-- | runs a moonbase expression in 'IO'
evalMoonbase :: (TVar Runtime) -> Moonbase a -> IO a
evalMoonbase ref (Moonbase a) = runReaderT a ref

-- | get current Theme
getTheme :: Moonbase Theme
getTheme = theme <$> get

-- | get current Config
getConfig :: Moonbase Config
getConfig = config <$> get

-- | lifts a 'IO' Action to 'Moonbase'
io :: IO a -> Moonbase a
io = liftIO


-- | Returns the moonbase configuration directory (usually ~/.config/moonbase)
getMoonbaseHomeDir :: IO FilePath
getMoonbaseHomeDir = do
    dir <- getUserConfigDir
    return $ dir </> "moonbase"

-- | Checks if the home config dir exists (~/.config/moonbase) if not create
setupHomeDirectory :: IO ()
setupHomeDirectory = do
 dir    <- getMoonbaseHomeDir
 exists <- doesDirectoryExist dir

 unless exists $ createDirectory dir

-- | Opens the log file
openLog :: IO Handle
openLog = do
 dir    <- getMoonbaseHomeDir
 exists <- doesFileExist (dir </> "moonbase" <.> "log")

 unless exists $ writeFile (dir </> "moonbase" <.> "log") ""
 openFile (dir </> "moonbase" <.> "log") WriteMode


-- |Execute moonbase
-- 
-- This function is the core of every moonbase configuration.
-- 
-- The second parameter are configuration methods which maybe adding some
-- components or hooks and ...
--
-- Basic example:
--
-- >   main :: IO
-- >   main = moonbase defaultConfig $ do
-- >     setTheme defaultTheme
-- >     withPreferred [ mimeImages ==> app "gimp"
-- >                   , mimeVideos ==> app "mplayer"
-- >                   , mimeSources ==> terminal "vim"]
-- >     withGtkPanel (xmonadLog <---> cpu <> systray <> clock)
-- >     withGtkPrompt (exec <> ssh) (cpuGraph `under` topStats)
-- >     withXMonad $ defaultMoonX {
-- >          terminal = "xterm"
-- >        , followMouse = True }
-- >     withBasicBackground defaultWallpaper
-- >     atStartup $ firefox <> pulseaudio <> xcompmgr <> dFeet
--
moonbase :: Config -> Moonbase () -> IO ()
moonbase conf decl = do

    args <- getArgs

    let isVerbose = "--verbose" `elem` args

    setupHomeDirectory
    handle  <- openLog
    client  <- startDBus
    signals <- atomically $ newTQueue
    runtime <- atomically $ newTVar $ newRuntime client handle conf signals

    startup runtime
    mainLoop runtime signals handle isVerbose

    where
 
        mainLoop rt ref handle isVerbose = do
            signal <- atomically $ readTQueue ref

            message <- formatMessage $ show signal
            
            liftIO $ hPutStrLn handle message >> hFlush handle
            when isVerbose $ hPutStrLn stderr message
 
            check rt ref handle signal isVerbose

        check rt ref handle signal isVerbose
          | isFatal signal    = return ()
          | isShutdown signal = shutdown rt >> mainLoop rt ref handle isVerbose
          | otherwise         = mainLoop rt ref handle isVerbose

        shutdown rt = forkIO $ evalMoonbase rt stopMoonbase
        startup  rt = forkIO $ evalMoonbase rt (decl >> startMoonbase)

formatMessage :: (MonadIO m) => String -> m String
formatMessage message = liftIO $ do
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> getZonedTime 
    return $ "[" ++ date ++ "] " ++ message

-- | Start components, run startup hooks and export basic DBus functionality           
startMoonbase :: Moonbase ()
startMoonbase = do
    push (Info "Starting moonbase")
    exportCoreDBus
    runHooks HookStart
    startComponents
    runHooks HookAfterStartup

-- | Export basic DBus functionality
exportCoreDBus :: Moonbase ()
exportCoreDBus = return ()

-- | Start components if they not allready started
startComponents :: Moonbase ()
startComponents = do
    rt <- get
    forMapM_ (comps rt) $ \k (Component init' mRef f _) ->
        case mRef of
             Just ref -> push (Warning $ "Component " ++ k ++ " is allready started")
             Nothing  -> do
                 push (Info $ "Starting " ++ k ++ "...")
                 ref <- liftIO $ atomically $ newTVar init'
                 evalComponentM k ref f              
    where
        forMapM_ map f = M.foldlWithKey (\_ k x -> f k x) (return ()) map

-- | Run cleanup and exit hooks
stopMoonbase :: Moonbase ()
stopMoonbase = do
    push (Info "Stoping moonbase")
    runCleanup
    runHooks HookExit
    push Exit

-- | push a signal to signal queue
--
-- for example:
--
-- > sample :: Moonbase ()
-- > sample = do
-- >      push (Info "some sample")
-- >      ...
-- >      push (Info "done")
push :: MSignal -> Moonbase ()
push signal = do
    runtime <- get
    liftIO $ atomically $ writeTQueue (signals runtime) signal

-- | Sets the moonbase theme
setTheme :: Theme -> Moonbase ()
setTheme t = modify (\rt -> rt { theme = t })

-- | Adds a hook to the runtime configuration
addHook :: Name -> HookType -> Moonbase () -> Moonbase ()
addHook name typ f = modify (\rt -> rt { hooks = new : (hooks rt)})
  where
      new = Hook name typ f

-- | Adds a cleanup method to runtime configuration
-- 
-- __Note:__ Do not rely on execution order of cleanup methods they
-- called in random order
addCleanup :: Moonbase () -> Moonbase ()
addCleanup cl = modify (\rt -> rt { cleanup = cl : (cleanup rt)})

-- | Register a component to moonbase runtime configuration
withComponent :: Name -> Component -> Moonbase ()
withComponent name comp = modify (\rt -> rt { comps = M.insert name comp (comps rt)})

-- | Get a component by name
getComponent :: Name -> Moonbase (Maybe Component)
getComponent n = M.lookup n . comps <$> get

-- | Modify a component
--
-- If the component is not found nothing is executed
modifyComponent :: Name -> (Component -> Component) -> Moonbase ()
modifyComponent n f = do
    mComp <- getComponent n
    case mComp of
         Nothing -> return ()
         Just c  -> withComponent n (f c)


runCleanup :: Moonbase ()
runCleanup = do
    rt <- get
    sequence_ $ (cleanup rt) ++ M.foldlWithKey runCF [] (comps rt) 
 where
     runCF xs k (Component _ (Just ref) _ c) = evalComponentM k ref c : xs
     runCF _  _ _                          = return $ return ()

-- | Specify at which state of execution a hook should be executed
data HookType = HookStart        -- ^ before moonbase starts
              | HookAfterStartup -- ^ after moonbase has started
              | HookExit         -- ^ after cleanup
              deriving(Show, Eq)

-- | A function which is executed by moonbase at the right time
data Hook = Hook Name HookType (Moonbase ())

-- | Helper function to generate a new Hook
makeHook :: Name -> HookType -> Moonbase () -> Hook
makeHook = Hook

runHooks :: HookType -> Moonbase ()
runHooks typ = mapM_ start =<< (filter (byType typ) . hooks <$> get)
    where
        byType a (Hook _ b _ ) = a == b
        start (Hook a ty call) = do
            push (Info $ "[" ++ show ty ++ "] hook '" ++ a ++ "' started")
            call


dbusListAllHooks :: Moonbase [(String,String)]
dbusListAllHooks
    = map (\(Hook n t _) -> (show t, n)) . hooks <$> get


-- | A 'ReaderT' transformer around 'Moonbase' and the state of the component
-- All components run in there own 'ComponentM'
-- 
-- 'ComponentM' derives 'MonadState' for easy accessing the state defined by
-- the component
--
-- The state is saved as a 'TVar' therefore it should be threadsafe
newtype ComponentM st a = ComponentM (ReaderT (Name, TVar st) Moonbase a)
    deriving (Functor, Monad, MonadIO, MonadReader (Name, TVar st))

instance Applicative (ComponentM st) where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (ComponentM st a) where
    mempty = return mempty
    mappend = liftM2 mappend

-- | Evaluates a component expression 
evalComponentM :: Name -> TVar st -> ComponentM st a -> Moonbase a
evalComponentM name state' (ComponentM cmd) = runReaderT cmd (name, state')

-- | Gets the acutal components name
componentName :: ComponentM st Name
componentName = fst <$> ask

-- | Generic definition of a component specifing the state and startup cleanup functions
data Component = forall st. Component st (Maybe (TVar st)) (ComponentM st ()) (ComponentM st ())

-- | Creates a new Component with an initial state 
newComponent :: st -> ComponentM st () -> Component
newComponent init' f = Component init' Nothing f (return ())

-- | Creates a new Component with an inital state and a cleanup function
newComponentWithCleanup :: st -> ComponentM st () -> ComponentM st () -> Component
newComponentWithCleanup init' f cl = Component init' Nothing f cl

-- | Lifts a 'Moonbase' action to ComponentM
moon :: Moonbase a -> ComponentM st a
moon = ComponentM . lift

-- | Adds a hook to moonbase runtime configuration
withHook :: Name -> HookType -> Moonbase () -> ComponentM st ()
withHook name typ f = moon $ addHook name typ f

-- | Signals a warning
warn :: String -> ComponentM st ()
warn msg = do
    name <- fst <$> ask
    moon $ push (Warning $ "[" ++ name ++ "]: " ++ msg)

-- | Prints a info
info :: String -> ComponentM st ()
info msg = do
    name <- fst <$> ask
    moon $ push (Info $ "[" ++ name ++ "]: " ++ msg)

-- | Signals a initialization failed
--
-- If the first parameter is True moonbase is forced to stop
initFailed :: Bool -> String -> ComponentM st ()
initFailed forceQuit msg = do
    name <- fst <$> ask
    moon $ push (InitFailed forceQuit $ "[" ++ name ++ "]: " ++ msg)


-- | The DBus bus name of moonbase
moonBusName :: BusName
moonBusName = "org.moonbase"

-- | The DBus interface name of moonbase
moonInterface :: InterfaceName
moonInterface = "org.moonbase"

startDBus :: IO Client
startDBus = do
        client <- connectSession
        name   <- requestName client moonBusName []
        case name of
            NamePrimaryOwner -> return client
            _                -> error "Connection to Session Bus failed. Name allready in use"


-- | Exports a DBus method under moonbase bus name
--
-- Using this function in combination with 'autoMethod' from @dbus@
-- should look like:
--
-- > someMethod :: String -> Moonbase Bool
-- > someMethod = null
-- > 
-- > addDBusMethod $ \rt -> 
-- >    autoMethod dbusInterfaceName "MethodsName" (wrap1 rt someMethod)
-- >
--
-- Use wrap0/1/2/3 to wrap methods with x arguments 
--
addDBusMethod :: ((TVar Runtime) -> Method)  -> Moonbase ()
addDBusMethod gen = do
    rt <- get  
    ref <- ask
    liftIO $ export (dbus rt) "/" [ gen ref ]

-- | Wraps a 'Moonbase b' to a 'IO' action
wrap0 :: (TVar Runtime) -> Moonbase b -> IO b
wrap0 ref cmd = evalMoonbase ref cmd

-- | Wraps a method with one argument to (a0 -> IO b)
wrap1 :: (IsValue a0) => (TVar Runtime) -> (a0 -> Moonbase b) -> a0 -> IO b
wrap1 ref cmd arg0 = evalMoonbase ref (cmd arg0)

-- | Wraps a method with two arguments to (a0 -> a1 -> IO b)
wrap2 :: (IsValue a0, IsValue a1) => (TVar Runtime) -> (a0 -> a1 -> Moonbase b) -> a0 -> a1 -> IO b
wrap2 ref cmd arg0 arg1 = evalMoonbase ref (cmd arg0 arg1)

-- | Wraps a method with three arguments to (a0 -> a1 -> a2 -> IO b)
wrap3 :: (IsValue a0, IsValue a1, IsValue a2) => (TVar Runtime) -> (a0 -> a1 -> a2 -> Moonbase b) -> a0 -> a1 -> a2 -> IO b
wrap3 ref cmd arg0 arg1 arg2 = evalMoonbase ref (cmd arg0 arg1 arg2)

-- | Adds a listing dbus signal to moonbase dbus client
addDBusSignal :: MatchRule -> (Signal -> Moonbase ()) -> Moonbase SignalHandler
addDBusSignal match cmd = do
    rt <- get
    ref <- ask
    liftIO $ addMatch (dbus rt) match $ \signal ->
      evalMoonbase ref (cmd signal)

-- | A map of mimetypes and there preferred application or action which should be
-- executed
data Preferred = forall a. (Executable a) => Preferred (M.Map String a)
