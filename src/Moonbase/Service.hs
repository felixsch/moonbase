module Moonbase.Service
    ( startServices
    , stopServices
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
    = mapM_ start =<< autostart <$> ask
    where
        start (Service n a) = do
            infoM $ "Starting service: " ++ n
            st <- get
            s <- startService a
            put $ st { services = M.insert n (Service n s) (services st) }
        start (OneShot n a) = infoM ("Enable oneshot service: "++n) >> enableService a


stopServices :: Moonbase ()
stopServices
    = mapM_ stop =<< (M.toList . services <$> get)
    where
        stop (n,Service _ a) = do
            infoM $ "Stoping service: " ++ n
            stopService a
            st <- get
            put $ st { services = M.delete n (services st) }
        stop (n, OneShot _ _) = throwError $ ErrorMessage $ "Trying to stop a oneshot service... wired! (" ++ n ++ " shoudl never be added here"






