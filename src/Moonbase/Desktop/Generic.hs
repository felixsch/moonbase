{-# LANGUAGE MultiParamTypeClasses #-}

module Moonbase.Desktop.Generic
    ( GenericDesktop(..)
    , newGenericDesktop
    , justBackgroundColor
    ) where

import Control.Monad.State

import Data.Maybe (isJust, fromJust)

import System.Process

import Moonbase.Core
import Moonbase.Log
import Moonbase.Util.Application


data GenericDesktop = GenericDesktop String [String] (Maybe ProcessHandle)

instance Requires GenericDesktop

instance StartStop GenericDesktop DesktopT where
    start = startGenericDesktop
    stop  = stopGenericDesktop
    isRunning = isGenericDesktopRunning
    
    
    
stopGenericDesktop :: DesktopT GenericDesktop ()
stopGenericDesktop 
    = do
        (GenericDesktop cmd _ hdl) <- get
        if isJust hdl
            then io $ terminateProcess $ fromJust hdl
            else debugM $ cmd ++ " is not running."

    
startGenericDesktop :: DesktopT GenericDesktop Bool 
startGenericDesktop
    = do
        (GenericDesktop cmd args hdl) <- get
        if isJust hdl
            then warnM (cmd ++ " is allready running.") >> return False
            else start' cmd args
    where
      start' c a =  do
        newHdl <- spawn c a
        case newHdl of
          Nothing -> errorM ("Could not start" ++ c) >> return False
          Just x  -> put (GenericDesktop c a (Just x)) >> return True

isGenericDesktopRunning :: DesktopT GenericDesktop Bool
isGenericDesktopRunning
    = do
        (GenericDesktop _ _ hdl) <- get
        return $ isJust hdl


newGenericDesktop :: String -> [String] -> Desktop
newGenericDesktop 
    cmd args = Desktop cmd $ GenericDesktop cmd args Nothing

justBackgroundColor :: String -> Desktop
justBackgroundColor color = newGenericDesktop "xsetroot" ["-solid", color]
