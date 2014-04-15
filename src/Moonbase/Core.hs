{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
MultiParamTypeClasses, TypeSynonymInstances, DeriveDataTypeable, OverloadedStrings#-}

module Moonbase.Core where



import Data.Monoid

import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import DBus.Client

import Moonbase.Util.Trigger


data MoonState = MoonState
  { quit :: Trigger
  , dbus :: Client
  }


data MoonConfig = MoonConfig {
 windowManager :: WindowManager

}

data MoonError = ErrorMessage String
                   | Quit

instance Error MoonError where
    noMsg  = ErrorMessage "Unknown Error"
    strMsg = ErrorMessage

newtype Moonbase a = Moonbase (ReaderT MoonConfig (StateT MoonState ( ErrorT MoonError IO)) a)
    deriving (Functor, Monad, MonadIO, MonadState MoonState, MonadReader MoonConfig)

instance Applicative Moonbase where
    pure    = return
    (<*>)   = ap

instance (Monoid a) => Monoid (Moonbase a) where
    mempty = return mempty
    mappend = liftM2 mappend


class WindowManagerClass a where
    startWM :: a -> Moonbase ()
    stopWM ::  a -> Moonbase ()

instance WindowManagerClass WindowManager where
    startWM (WindowManager wm) = startWM wm
    stopWM (WindowManager wm)  = stopWM wm

data WindowManager = forall a. (WindowManagerClass a) => WindowManager a


    
runMoon :: MoonConfig -> MoonState -> Moonbase a -> IO (Either MoonError (a, MoonState))
runMoon
    conf st (Moonbase a) = runErrorT $ runStateT (runReaderT a conf) st


io :: (MonadIO m) => IO a -> m a
io
    = liftIO



startDbusSession :: IO Client
startDbusSession 
    = do
        client <- connectSession
        name   <- requestName client "org.Moonbase" []
        case name of
            NamePrimaryOwner -> return client
            _                -> error "Connection to Session Bus failed. Name allready in use"


registerDBusQuit :: Moonbase ()
registerDBusQuit = do
            st <- get
            io $ export (dbus st) "/org/moonbase" [ autoMethod "org.Moonbase.Core" "Quit" (runQuit st)]
        where
            runQuit = trigger . quit

moonbase :: MoonConfig -> IO ()
moonbase
    conf = do
            client <- startDbusSession
            q <- newTrigger
            re <- runMoon conf (MoonState q client) exec
            case re of
                Left err -> handleError err
                Right _  -> putStrLn "Bye.."
    where
        exec = registerDBusQuit <> startWM' <> mainLoop
        startWM' = startWM . windowManager =<< ask
        mainLoop = do
            st <- get
            io $ waitUntil (quit st)
        handleError (ErrorMessage err) = putStrLn $ "Error: " ++ err
        handleError _                  = putStrLn "Unknown Error"
            
