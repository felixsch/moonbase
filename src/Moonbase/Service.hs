module Moonbase.Service
    ( startServices
    , stopServices
    , startService
    , stopService
    , askService
    , isServiceRunning
    , dbusListAllServices
    , dbusListRunningServices
    , dbusStopService
    , dbusStartService
    ) where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

import Moonbase.Log
import Moonbase.Core


startService :: Service -> Moonbase ()
startService (Service n _ st) = do
        infoM $ "Starting service: " ++ n
        ref <- newRef st
        status <- runComponentM n ref start
        if status
            then modify (\x -> x { stServices = M.insert n (RefWrapper ref) (stServices x)})
            else warnM $ "Starting service: " ++ n ++ " failed!"

stopService :: Name -> Moonbase ()
stopService n = do
    runtime <- get
    infoM $ "Stoping service: " ++ n
    case (M.lookup n (stServices runtime)) of
        Just _                -> modify (\x -> x { stServices = M.delete n (stServices runtime)})
        Nothing               -> debugM $ "Trying to stop a non existing service: " ++ n


startServices :: Moonbase ()
startServices = mapM_ startService =<< autostart <$> askConf


stopServices :: Moonbase ()
stopServices
    = mapM_ stopService =<< (M.keys . stServices <$> get)



askService :: Name -> Moonbase (Maybe Service)
askService 
    sn = search . autostart <$> askConf
    where
        search (x@(Service n _ _):xs)
            | n  == sn = Just x
            | otherwise = search xs
        search [] = Nothing


dbusListAllServices :: Moonbase [String]
dbusListAllServices
    = map (\(Service n _ _) -> n) . autostart <$> askConf


dbusListRunningServices :: Moonbase [String]
dbusListRunningServices
    = M.keys . stServices <$> get


dbusStopService :: Name -> Moonbase ()
dbusStopService = stopService

isServiceRunning :: Name -> Moonbase Bool
isServiceRunning
    sn = M.member sn . stServices <$> get


dbusStartService :: Name -> Moonbase ()
dbusStartService
    sn = do
        notRunning <- not <$> isServiceRunning sn
        when notRunning (maybe (return ()) startService =<< askService sn)
