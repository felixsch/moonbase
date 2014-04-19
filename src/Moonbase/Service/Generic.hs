module Moonbase.Service.Generic where

import Control.Monad (void)
import Control.Applicative
import System.Process

import Data.Maybe (isNothing)

import Moonbase.Core
import Moonbase.Util.Application


data GenericService = GenericService String [String] (Maybe ProcessHandle)

instance StartStop GenericService where
    startService  = startGenericService
    stopService   = stopGenericService
    isRunning     = isGenericServiceRunning



startGenericService :: GenericService -> Moonbase GenericService
startGenericService
    (GenericService cmd args Nothing) = GenericService cmd args . Just <$> spawn cmd args

stopGenericService :: GenericService -> Moonbase ()
stopGenericService
    (GenericService _ _ (Just pr)) = io (terminateProcess pr)


isGenericServiceRunning :: GenericService -> Moonbase Bool
isGenericServiceRunning
    (GenericService _ _ (Just pr)) = isNothing <$> io (getProcessExitCode pr)
isGenericServiceRunning
    _                              = return False



data GenericOneShot = GenericOneShot String [String]

instance Enable GenericOneShot where
    enableService (GenericOneShot cmd args) = void $ spawn cmd args
