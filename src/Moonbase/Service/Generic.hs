module Moonbase.Service.Generic
    ( GenericService(..)
    , newGenericService
    , newGenericOneShot
    ) where

import Control.Monad (void)
import Control.Applicative
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



startGenericService :: GenericService -> Moonbase GenericService
startGenericService
    (GenericService cmd args Nothing) = GenericService cmd args . Just <$> spawn cmd args

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



data GenericOneShot = GenericOneShot String [String]

instance Enable GenericOneShot where
    enable (GenericOneShot cmd args) = void $ spawn cmd args

newGenericService :: String -> [String] -> Service
newGenericService 
    cmd args = Service cmd $ GenericService cmd args Nothing


newGenericOneShot :: String -> [String] -> Service
newGenericOneShot
    cmd args = OneShot cmd $ GenericOneShot cmd args
