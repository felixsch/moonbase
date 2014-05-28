module Moonbase.Service.Generic
    ( GenericService(..)
    , newGenericService
    ) where

import Control.Applicative
import Control.Monad.Error (throwError)

import System.Process


import Data.Maybe (isNothing)

import Moonbase.Core
import Moonbase.Log
import Moonbase.Util.Application


data GenericService = GenericService String [String] (Maybe ProcessHandle)

instance StartStop GenericService where
    start  = startGenericService
    stop   = stopGenericService
    isRunning     = isGenericServiceRunning

instance Requires GenericService


startGenericService :: GenericService -> Moonbase GenericService
startGenericService
    (GenericService cmd args Nothing) = GenericService cmd args . Just <$> spawn cmd args
startGenericService
    (GenericService cmd _    _      ) = throwError $ ErrorMessage $ "Trying to call a allready started serice: " ++ cmd 

stopGenericService :: GenericService -> Moonbase ()
stopGenericService
    (GenericService _ _ (Just pr)) = io (terminateProcess pr)
stopGenericService
    (GenericService n _ _)         = warnM $ "service: " ++ n ++ " is not running but should be stopped"


isGenericServiceRunning :: GenericService -> Moonbase Bool
isGenericServiceRunning
    (GenericService _ _ (Just pr)) = isNothing <$> io (getProcessExitCode pr)
isGenericServiceRunning
    _                              = return False


newGenericService :: String -> [String] -> Service
newGenericService 
    cmd args = Service cmd $ GenericService cmd args Nothing
