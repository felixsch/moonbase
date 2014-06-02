{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
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
    -- NEW
    , NService(..)
    ) where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error


import Data.Monoid
import Data.Maybe
import qualified Data.Map as M

import Moonbase.Core
import Moonbase.Log



-- NEW

data ServiceError = ServiceError String
                  | ServiceWarning  String
                  | ServiceFatalError String Bool

instance Error ServiceError where
    noMsg  = ServiceError "A unknown error occured!"
    strMsg = ServiceError


newtype ServiceT st m a = ServiceT (StateT st (ErrorT ServiceError m) a)
    deriving (Functor, Monad, MonadIO, MonadState st, MonadError ServiceError)

instance (Monad m, Monoid a) => Monoid (ServiceT m st a) where
    mempty = return mempty
    mappend = liftM2 mappend
   
instance Applicative (ServiceT st) where
    pure = return
    (<*>) = ap

class (Monad m) => NewService m st where

    initState :: ServiceT st m st

    startService :: ServiceT st m Bool
    stopService  :: ServiceT st m ()
    
    restartService :: ServiceT st m Bool 

    isServiceRunning :: ServiceT st m Bool



runServiceT :: ServiceT st a -> st -> Moonbase (a, st)
runServiceT (ServiceT cmd) = runStateT cmd

nstartServices :: [NService st] -> Moonbase [NService st]
nstartServices s = catMaybes <$> mapM startS s
    where
      startS (NService n i) = do
        (is, sta) <- runServiceT nstart i
        return $ if is then Just $ NService n sta else Nothing



data SampleService = Int

instance NStartStop (ServiceT SampleService) where
    nstart = modify ((+1) :: Int -> Int) >> return True
    nstop  = return ()
    nisRunning = return True

newSample :: Int -> NService SampleService
newSample = NService "IntegerSample"




data Sample = Sample
  { a :: String
  , b :: forall st. [NService st]
  }

sample :: Sample
sample = Sample
    { a = "Moep"
    , b = [newSample 12]
    }


-- END



startServices :: Moonbase ()
startServices
    = mapM_ start' =<< autostart <$> askConf
    where
        start' (Service n a) = do
            infoM $ "Starting service: " ++ n
            st <- get
            s <- start a
            put $ st { services = M.insert n (Service n s) (services st) }


stopServices :: Moonbase ()
stopServices
    = mapM_ stop' =<< (M.toList . services <$> get)
    where
        stop' (n,Service _ a) = do
            infoM $ "Stoping service: " ++ n
            stop a
            st <- get
            put $ st { services = M.delete n (services st) }


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
