module Moonbase.Service.Generic
    ( GenericService(..)
    , newGenericService
    ) where

import Control.Applicative
import Control.Monad.State

import System.Process


import Data.Maybe (isJust)

import Moonbase.Core
import Moonbase.Util.Application



data GenericService = GenericService String [String] (Maybe ProcessHandle)

instance Requires GenericService

instance IsService GenericService where
    initState = return $ GenericService "" [] Nothing
    startService = do
        (GenericService cmd args _) <- get
        hdl <- spawn cmd args
        put $ GenericService cmd args hdl
        return True
    stopService = do
        (GenericService _  _ hdl) <- get
        maybe (return ()) (io . terminateProcess) hdl

    isServiceRunning = do
        (GenericService _ _ hdl) <- get
        isJust <$> getExitCode hdl
        where
            getExitCode (Just x) = io $ getProcessExitCode x
            getExitCode Nothing  = return Nothing

newGenericService :: String -> [String] -> Service
newGenericService 
    cmd args = Service cmd $ GenericService cmd args Nothing
