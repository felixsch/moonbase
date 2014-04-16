{-# LANGUAGE OverloadedStrings #-}
module Moonbase 
    ( moonbase
    ) where



import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import DBus.Client

import Moonbase.Util.Trigger

import Moonbase.WindowManager
import Moonbase.Core



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

startMoonbase :: Moonbase ()
startMoonbase
    = registerDBusQuit >> startWindowManager

stopMoonbase :: Moonbase ()
stopMoonbase
    = stopWindowManager

moonbase :: MoonConfig -> IO ()
moonbase
    conf = do
            client <- startDbusSession
            q <- newTrigger
            re <- runMoon conf (MoonState q client Nothing) exec
            case re of
                Left err -> handleError err
                Right _  -> putStrLn "Bye.."
    where
        exec = startMoonbase >> mainLoop >> stopMoonbase
        mainLoop = do
            st <- get
            io $ waitUntil (quit st)
        handleError (ErrorMessage err) = putStrLn $ "Error: " ++ err
        handleError _                  = putStrLn "Unknown Error"
            

