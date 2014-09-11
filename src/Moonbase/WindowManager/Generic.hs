{-# LANGUAGE MultiParamTypeClasses #-}

module Moonbase.WindowManager.Generic
    ( GenericWM(..)
    , newGenericWM
    ) where

import Control.Applicative
import Control.Monad.State

import Data.Maybe (isJust, fromJust) 

import System.Process

import Moonbase.Core
import Moonbase.Log
import Moonbase.Util.Application

data GenericWM = GenericWM String [String] (Maybe ProcessHandle)

instance Component GenericWM where
    start = do
        (GenericWM w args _) <- get
        hdl <- spawn w args
        if isJust hdl
            then put (GenericWM w args hdl) >> return True
            else errorM ("Could not start generic windowmanager " ++ w) >> return False

    stop = do
        (GenericWM w _ hdl) <- get
        if isJust hdl
            then io $ terminateProcess $ fromJust hdl
            else warnM $ "Could not stop generic windowmanager " ++ w ++ ": WM was not started"

    isRunning = isJust . handle <$> get
        where
            handle (GenericWM _ _ hdl) = hdl

newGenericWM :: String -> [String] -> WindowManager
newGenericWM
    cmd args = WindowManager cmd [] $ GenericWM cmd args Nothing



