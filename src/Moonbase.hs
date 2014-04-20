{-# LANGUAGE OverloadedStrings #-}
module Moonbase 
    ( moonbase
    ) where




import Control.Monad.State

import qualified Data.Map as M

import DBus.Client

import System.IO
import System.Environment.XDG.BaseDir

import Moonbase.Util.Trigger

import Moonbase.Core
import Moonbase.Log
import Moonbase.Service
import Moonbase.Preferred
import Moonbase.WindowManager


moonHome :: IO FilePath
moonHome = do
    dir <- getUserConfigDir
    return $ dir ++ "/moonbase/"


startDbus :: IO Client
startDbus 
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
     dir <- moonHome
     openFile (dir ++ "moonbase.log") WriteMode

        
newMoonState :: Client -> Handle -> IO MoonState
newMoonState
    client hdl = do
        q <- newTrigger
        return MoonState
            { quit   = q
            , dbus   = client
            , wm     = Nothing
            , logHdl = hdl
            , services = M.empty
            }

startMoonbase :: Moonbase ()
startMoonbase
    = infoM "Starting moonbase..." >> registerDBusQuit >> setPreferred >> startWindowManager >> startServices

stopMoonbase :: Moonbase ()
stopMoonbase
    = stopServices >> stopWindowManager >> infoM "Stoping moonbase..."

moonbase :: MoonConfig -> IO ()
moonbase
    conf = do
            client <- startDbus
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
        handleError (AppNotFound a)  = putStrLn $ "APP NOT FOUND: " ++ a
        handleError _                  = putStrLn "Unknown Error"
            

