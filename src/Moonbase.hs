{-# LANGUAGE OverloadedStrings #-}
module Moonbase 
    ( moonbase
    ) where



import Control.Applicative
import Control.Monad.State

import qualified Data.Map as M

import DBus.Client
import Data.IORef

import System.IO
import System.Environment.XDG.BaseDir

import Moonbase.Util.Trigger

import Moonbase.Core
import Moonbase.Log
import Moonbase.Hook
import Moonbase.Panel
import Moonbase.Service
import Moonbase.Preferred
import Moonbase.WindowManager
import Moonbase.Desktop


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

registerDBus :: Moonbase ()
registerDBus
    = do
        ref <- askRef
        st <- get
        cf <- askConf        
        io $ export (dbus st) "/org/moonbase"
            [ autoMethod core "Quit" (trigger $ quit st)
            , autoMethod core "ListRunningServices" (wrap cf ref dbusListRunningServices)
            , autoMethod core "ListAllServices" (wrap cf ref dbusListAllServices)
            , autoMethod core "StopService" (wrap1 cf ref dbusStopService)
            , autoMethod core "StartService" (wrap1 cf ref dbusStartService)
            , autoMethod core "ListAllHooks" (wrap cf ref dbusListAllHooks)
            , autoMethod core "ListRunningPanels" (wrap cf ref dbusListRunningPanels)
            , autoMethod core "ListAllPanels" (wrap cf ref dbusListAllPanels)
            , autoMethod core "StartPanel" (wrap1 cf ref dbusStartPanel)
            , autoMethod core "StopPanel" (wrap1 cf ref dbusStopPanel)
            ]
    where
        core = "org.Moonbase.Core"
        wrap cf ref cmd = justReturn <$> runMoon cf ref cmd
        wrap1 cf ref cmd arg = justReturn <$> runMoon cf ref (cmd arg)

justReturn :: Either MoonError a -> a
justReturn (Left (ErrorMessage err)) = error $ "Error: " ++ err
justReturn (Left (AppNotFound appl)) = error $ "App not found: " ++ appl
justReturn (Right x) = x
 
            
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
            , desk   = Nothing
            , logHdl = hdl
            , logVerbose = True
            , services = M.empty
            , pnls = M.empty
            , hks  = []
            }

startMoonbase :: Moonbase ()
startMoonbase
    = infoM "Starting moonbase..." 
    >> loadHooks
    >> runHooks HookStart
    >> registerDBus
    >> setPreferred
    >> startDesktop
    >> startWindowManager
    >> startServices
    >> startPanels
    >> runHooks HookAfterStartup

stopMoonbase :: Moonbase ()
stopMoonbase
    = runHooks HookBeforeQuit
    >> stopPanels
    >> stopServices 
    >> stopWindowManager 
    >> stopDesktop 
    >> runHooks HookQuit
    >> infoM "Stoping moonbase..."

moonbase :: MoonConfig -> IO ()
moonbase
    conf = do
            client <- startDbus
            l <- openLog
            st <- newIORef =<< newMoonState client l
            re <- runMoon conf st exec
            case re of
                Left err -> handleError err
                Right _  -> putStrLn "Bye.."
    where
        exec = startMoonbase >> mainLoop >> stopMoonbase
        mainLoop = do
            st <- get
            io $ waitUntil (quit st)
            
handleError :: MoonError -> IO ()                
handleError (ErrorMessage err)
    = putStrLn $ "Error: " ++ err
handleError (AppNotFound a) 
    = putStrLn $ "APP NOT FOUND: " ++ a
handleError _
    = putStrLn "Unknown Error"


