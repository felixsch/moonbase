{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

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

import Moonbase.Log
import Moonbase.Core


startService' :: Service -> Moonbase ()
startService' 
    (Service n a) = do
        infoM $ "Starting service: " ++ n
        old <- get
        sta <- runServiceT startService a
        case sta of 
            Left err            -> handleServiceError err
            Right (started, st) -> ifStarted n started st old

    where
        ifStarted na True st old = put $ old { services = M.insert na (Service n st) (services old) }
        ifStarted na False _ _   = warnM $ " -- starting service " ++ na ++ " failed!"


stopService' :: Service -> Moonbase ()
stopService' (Service n a) = do
    infoM $ "Stoping service: " ++ n
    _ <- runServiceT stopService a
    st <- get
    put $ st { services = M.delete n (services st) }


startServices :: Moonbase ()
startServices = mapM_ startService' =<< autostart <$> askConf


stopServices :: Moonbase ()
stopServices
    = mapM_ stopService' =<< (M.elems . services <$> get)


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

handleServiceError :: ServiceError -> Moonbase ()
handleServiceError (ServiceError msg)      = errorM $ "  (EE) " ++ msg
handleServiceError (ServiceWarning msg)    = warnM $ " (!!) " ++ msg
handleServiceError (ServiceFatalError msg) = throwError $ FatalError msg

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
            stopService' s
            put $ st { services = M.delete sn (services st) }

iisServiceRunning :: Name -> Moonbase Bool
iisServiceRunning
    sn = M.member sn . services <$> get
dbusStartService :: Name -> Moonbase ()
dbusStartService
    sn = do
        notRunning <- not <$> iisServiceRunning sn
        when notRunning (maybe (return ()) startService' =<< askService sn)
