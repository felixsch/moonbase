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
import System.Environment.XDG.DesktopEntry

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


import Moonbase.Util.Application


import DBus.Client
import DBus



-- Signal --------------------------------------------------
data MSignal = Shutdown
            | Exit
            | ApplicationNotFound String
            | FatalError String
            | Warning String
            | Info String
            | Message String
            | InitFailed Bool String

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

isFatal :: MSignal -> Bool
isFatal Shutdown                 = False
isFatal (Message _)              = False
isFatal (Warning _)              = False
isFatal (Info _)                 = False
isFatal (InitFailed forceQuit _) = forceQuit
isFatal _                        = True

isShutdown :: MSignal -> Bool 
isShutdown Shutdown = True
isShutdown _        = False

-- Basic types --------------------------------------------------
--
type Name = String

data Theme   = Theme {}
data Config  = Config {}

data Runtime = Runtime 
    { dbus        :: Client
    , config      :: Config
    , signals     :: TQueue MSignal
    , comps       :: M.Map Name Component
    , preferred   :: Maybe Preferred
    , cleanup     :: [Moonbase ()]
    , hooks       :: [Hook]
    }

newRuntime :: Client -> Config -> TQueue MSignal -> Runtime
newRuntime client conf sigs = Runtime
  { dbus        = client
  , config      = conf
  , signals     = sigs 
  , comps       = M.empty
  , preferred   = Nothing
  , cleanup     = []
  , hooks       = []
  }


-- Moonbase --------------------------------------------------
type MoonbaseT = ReaderT (TVar Runtime) IO

newtype Moonbase a = Moonbase { runMoonbase :: MoonbaseT a }
    deriving (Functor, Monad, MonadIO, MonadReader (TVar Runtime))

instance MonadBase IO Moonbase where
    liftBase = Moonbase . lift

instance MonadBaseControl IO Moonbase where
    newtype StM Moonbase a = StMoonbase { unStMoonbase :: StM MoonbaseT a }
    liftBaseWith f = Moonbase (liftBaseWith (\runIO -> f (fmap StMoonbase . runIO . runMoonbase)))
    restoreM = Moonbase . restoreM . unStMoonbase

instance Applicative Moonbase where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (Moonbase a) where
    mempty = return mempty
    mappend = liftM2 mappend


instance MonadState Runtime Moonbase where
    get = do
        ref <- ask
        liftIO $ readTVarIO ref
    put sta = do
        ref <- ask
        liftIO $ atomically $ writeTVar ref sta

evalMoonbase :: (TVar Runtime) -> Moonbase a -> IO a
evalMoonbase ref (Moonbase a) = runReaderT a ref

io :: IO a -> Moonbase a
io = liftIO

moonbase :: Config -> Moonbase () -> IO ()
moonbase conf decl = do
    client  <- startDBus
    signals <- atomically $ newTQueue
    runtime <- atomically $ newTVar $ newRuntime client conf signals

    startup runtime
    mainLoop runtime signals

    where
        mainLoop rt ref = do
            signal <- atomically $ readTQueue ref
            putStrLn $ show signal
            check rt ref signal

        check rt ref signal
          | isFatal signal    = return ()
          | isShutdown signal = shutdown rt >> mainLoop rt ref
          | otherwise         = mainLoop rt ref

        shutdown rt = forkIO $ evalMoonbase rt stopMoonbase
        startup  rt = forkIO $ evalMoonbase rt (decl >> startMoonbase)

            
startMoonbase :: Moonbase ()
startMoonbase = do
    push (Info "Starting moonbase")
    exportCoreDBus
    runHooks HookStart
    startComponents
    runHooks HookAfterStartup

exportCoreDBus :: Moonbase ()
exportCoreDBus = return ()

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

stopMoonbase :: Moonbase ()
stopMoonbase = do
    push (Info "Stoping moonbase")
    runCleanup
    runHooks HookExit
    push Exit

push :: MSignal -> Moonbase ()
push signal = do
    runtime <- get
    liftIO $ atomically $ writeTQueue (signals runtime) signal

addHook :: Name -> HookType -> Moonbase () -> Moonbase ()
addHook name typ f = modify (\rt -> rt { hooks = new : (hooks rt)})
  where
      new = Hook name typ f

addCleanup :: Moonbase () -> Moonbase ()
addCleanup cl = modify (\rt -> rt { cleanup = cl : (cleanup rt)})

withComponent :: Name -> Component -> Moonbase ()
withComponent name comp = modify (\rt -> rt { comps = M.insert name comp (comps rt)})

getComponent :: Name -> Moonbase (Maybe Component)
getComponent n = M.lookup n . comps <$> get

modifyComponent :: Name -> (Component -> Component) -> Moonbase ()
modifyComponent n f = do
    mComp <- getComponent n
    case mComp of
         Nothing -> return ()
         Just c  -> withComponent n (f c)

-- Cleanup -----------------------------------------------

runCleanup :: Moonbase ()
runCleanup = do
    rt <- get
    sequence_ $ (cleanup rt) ++ M.foldlWithKey runCF [] (comps rt) 
 where
     runCF xs k (Component _ (Just ref) _ c) = evalComponentM k ref c : xs
     runCF _  _ _                          = return $ return ()


-- Hook --------------------------------------------------
data HookType = HookStart
              | HookAfterStartup
              | HookExit
              deriving(Show, Eq)


data Hook = Hook Name HookType (Moonbase ())

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


-- ComponentM --------------------------------------------------

newtype ComponentM st a = ComponentM (ReaderT (Name, TVar st) Moonbase a)
    deriving (Functor, Monad, MonadIO, MonadReader (Name, TVar st))

instance Applicative (ComponentM st) where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (ComponentM st a) where
    mempty = return mempty
    mappend = liftM2 mappend


evalComponentM :: Name -> TVar st -> ComponentM st a -> Moonbase a
evalComponentM name state' (ComponentM cmd) = runReaderT cmd (name, state')

componentName :: ComponentM st Name
componentName = fst <$> ask

data Component = forall st. Component st (Maybe (TVar st)) (ComponentM st ()) (ComponentM st ())

newComponent :: st -> ComponentM st () -> Component
newComponent init' f = Component init' Nothing f (return ())

newComponentWithCleanup :: st -> ComponentM st () -> ComponentM st () -> Component
newComponentWithCleanup init' f cl = Component init' Nothing f cl


-- newComponentWithCleanup :: Component st st -> Component st () -> Component

moon :: Moonbase a -> ComponentM st a
moon = ComponentM . lift

withHook :: Name -> HookType -> Moonbase () -> ComponentM st ()
withHook name typ f = moon $ addHook name typ f

warn :: String -> ComponentM st ()
warn msg = do
    name <- fst <$> ask
    moon $ push (Warning $ "[" ++ name ++ "]: " ++ msg)

info :: String -> ComponentM st ()
info msg = do
    name <- fst <$> ask
    moon $ push (Info $ "[" ++ name ++ "]: " ++ msg)

initFailed :: Bool -> String -> ComponentM st ()
initFailed forceQuit msg = do
    name <- fst <$> ask
    moon $ push (InitFailed forceQuit $ "[" ++ name ++ "]: " ++ msg)




-- DBus --------------------------------------------------

moonBusName :: BusName
moonBusName = "org.moonbase"

moonInterface :: InterfaceName
moonInterface = "org.moonbase"

startDBus :: IO Client
startDBus = do
        client <- connectSession
        name   <- requestName client moonBusName []
        case name of
            NamePrimaryOwner -> return client
            _                -> error "Connection to Session Bus failed. Name allready in use"


addDBusMethod :: ((TVar Runtime) -> Method)  -> Moonbase ()
addDBusMethod gen = do
    rt <- get  
    ref <- ask
    liftIO $ export (dbus rt) "/" [ gen ref ]

wrap0 :: (TVar Runtime) -> Moonbase b -> IO b
wrap0 ref cmd = evalMoonbase ref cmd

wrap1 :: (IsValue a0) => (TVar Runtime) -> (a0 -> Moonbase b) -> a0 -> IO b
wrap1 ref cmd arg0 = evalMoonbase ref (cmd arg0)

wrap2 :: (IsValue a0, IsValue a1) => (TVar Runtime) -> (a0 -> a1 -> Moonbase b) -> a0 -> a1 -> IO b
wrap2 ref cmd arg0 arg1 = evalMoonbase ref (cmd arg0 arg1)

wrap3 :: (IsValue a0, IsValue a1, IsValue a2) => (TVar Runtime) -> (a0 -> a1 -> a2 -> Moonbase b) -> a0 -> a1 -> a2 -> IO b
wrap3 ref cmd arg0 arg1 arg2 = evalMoonbase ref (cmd arg0 arg1 arg2)

addDBusSignal :: MatchRule -> (Signal -> Moonbase ()) -> Moonbase SignalHandler
addDBusSignal match cmd = do
    rt <- get
    ref <- ask
    liftIO $ addMatch (dbus rt) match $ \signal ->
      evalMoonbase ref (cmd signal)

---- preferred ---------------------------------------------------------------------

data Preferred = forall a. (Executable a) => Preferred (M.Map String a)
