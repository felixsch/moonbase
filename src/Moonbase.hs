{-# LANGUAGE OverloadedStrings #-}
module Moonbase 
    ( moonbase
    ) where



import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import DBus.Client

import System.IO
import System.Environment.XDG.BaseDir

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


openLog :: IO Handle
openLog
    = do
     dir <- getUserConfigDir
     openFile (dir ++ "/moonbase.log") WriteMode

        
newMoonState :: Client -> Handle -> IO MoonState
newMoonState
    client hdl = do
        q <- newTrigger
        return MoonState
            { quit   = q
            , dbus   = client
            , wm     = Nothing
            , logHdl = hdl
            }

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
            l <- openLog
            st <- newMoonState client l
            re <- runMoon conf st exec
            case re of
                Left err -> handleError err
                Right _  -> putStrLn "Bye.."
    where
        exec = startMoonbase >> mainLoop >> stopMoonbase
        mainLoop = do
            st <- get
            io $ waitUntil (quit st)
        handleError (ErrorMessage err) = putStrLn $ "Error: " ++ err
        handleError (AppNotFound app)  = putStrLn $ "APP NOT FOUND: " ++ app
        handleError _                  = putStrLn "Unknown Error"
            

