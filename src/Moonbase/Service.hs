module Moonbase.Service
    ( startServices
    , stopServices
    , getService, putService
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
import Control.Monad.Error

import qualified Data.Map as M

import Moonbase.Core
import Moonbase.Log

startServices :: Moonbase ()
startServices
    = mapM_ start' =<< autostart <$> askConf
    where
        start' (Service n a) = do
            infoM $ "Starting service: " ++ n
            st <- get
            s <- start a
            put $ st { services = M.insert n (Service n s) (services st) }
        start' (OneShot n a) = infoM ("Enable oneshot service: "++n) >> enable a


stopServices :: Moonbase ()
stopServices
    = mapM_ stop' =<< (M.toList . services <$> get)
    where
        stop' (n,Service _ a) = do
            infoM $ "Stoping service: " ++ n
            stop a
            st <- get
            put $ st { services = M.delete n (services st) }
        stop' (n, OneShot _ _) = throwError $ ErrorMessage $ "Trying to stop a oneshot service... wired! (" ++ n ++ " shoudl never be added here"


getService :: Name -> Moonbase (Maybe Service)
getService
    n = M.lookup n . services <$> get

putService :: Service -> Moonbase ()
putService
    s@(Service n _) = modify (\st -> st { services = M.insert n s (services st)})

askService :: Name -> Moonbase (Maybe Service)
askService 
    sn = search . autostart <$> askConf
    where
        search (x@(Service n _):xs)
            | n  == sn = Just x
            | otherwise = search xs
        search [] = Nothing

isServiceRunning :: Name -> Moonbase Bool
isServiceRunning
    sn = M.member sn . services <$> get

dbusListAllServices :: Moonbase [String]
dbusListAllServices
    = map (\(Service n _) -> n) . autostart <$> askConf


dbusListRunningServices :: Moonbase [String]
dbusListRunningServices
    = M.keys . services <$> get


dbusStopService :: Name -> Moonbase ()
dbusStopService
    sn = perform =<< M.lookup sn <$> (services <$> get)
    where
        perform Nothing  = return ()
        perform (Just s) = do
            st <- get
            stop s
            put $ st { services = M.delete sn (services st) }

dbusStartService :: Name -> Moonbase ()
dbusStartService
    sn = do
        notRunning <- not <$> isServiceRunning sn
        when notRunning (start' =<< askService sn)
    where
        start' Nothing  = warnM $ "Could not start service: " ++ sn ++ " is unknown"
        start' (Just s) = do
            debugM $ "dbus --> starting service " ++ sn
            ns <- start s
            putService $ Service sn ns
