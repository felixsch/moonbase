module Moonbase.Service
    ( startServices
    , stopServices
    , startService
    , stopService
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

import Moonbase.Log
import Moonbase.Core


startService :: Service -> Moonbase ()
startService 
    (Service n a) = do
        infoM $ "Starting service: " ++ n
        old <- get
        sta <- runServiceT start a
        case sta of 
            Left err            -> handleServiceError err
            Right (started, st) -> ifStarted n started st old

    where
        ifStarted na True st old = put $ old { stServices = M.insert na (Service n st) (stServices old) }
        ifStarted na False _ _   = warnM $ " -- starting service " ++ na ++ " failed!"


stopService :: Service -> Moonbase ()
stopService (Service n a) = do
    infoM $ "Stoping service: " ++ n
    _ <- runServiceT stop a
    st <- get
    put $ st { stServices = M.delete n (stServices st) }


startServices :: Moonbase ()
startServices = mapM_ startService =<< autostart <$> askConf


stopServices :: Moonbase ()
stopServices
    = mapM_ stopService =<< (M.elems . stServices <$> get)


getService :: Name -> Moonbase (Maybe Service)
getService
    n = M.lookup n . stServices <$> get

putService :: Service -> Moonbase ()
putService
    s@(Service n _) = modify (\st -> st { stServices = M.insert n s (stServices st)})

askService :: Name -> Moonbase (Maybe Service)
askService 
    sn = search . autostart <$> askConf
    where
        search (x@(Service n _):xs)
            | n  == sn = Just x
            | otherwise = search xs
        search [] = Nothing

handleServiceError :: ServiceError -> Moonbase ()
handleServiceError (ServiceError msg)      = errorM $ "  (EE) " ++ msg
handleServiceError (ServiceWarning msg)    = warnM $ " (!!) " ++ msg
handleServiceError (ServiceFatalError msg) = throwError $ FatalError msg

dbusListAllServices :: Moonbase [String]
dbusListAllServices
    = map (\(Service n _) -> n) . autostart <$> askConf


dbusListRunningServices :: Moonbase [String]
dbusListRunningServices
    = M.keys . stServices <$> get


dbusStopService :: Name -> Moonbase ()
dbusStopService
    sn = perform =<< M.lookup sn <$> (stServices <$> get)
    where
        perform Nothing  = return ()
        perform (Just s) = do
            st <- get
            stopService s
            put $ st { stServices = M.delete sn (stServices st) }

isServiceRunning :: Name -> Moonbase Bool
isServiceRunning
    sn = M.member sn . stServices <$> get


dbusStartService :: Name -> Moonbase ()
dbusStartService
    sn = do
        notRunning <- not <$> isServiceRunning sn
        when notRunning (maybe (return ()) startService =<< askService sn)
