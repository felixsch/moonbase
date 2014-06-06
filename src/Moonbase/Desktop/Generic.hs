{-# LANGUAGE MultiParamTypeClasses #-}

module Moonbase.Desktop.Generic
    ( GenericDesktop(..)
    , newGenericDesktop
    , justBackgroundColor
    ) where

import Control.Monad.State
import Control.Applicative

import Data.Maybe (isJust, fromJust)

import System.Process

import Moonbase.Core
import Moonbase.Log
import Moonbase.Util.Application


data GenericDesktop = GenericDesktop String [String] (Maybe ProcessHandle)


-- new NStartStop instance

instance NStartStop GenericDesktop DesktopT where
    ninitState = return $ GenericDesktop "" [] Nothing
    nstart = nStartGenericDesktop
    nstop  = nStopGenericDesktop
    nisRunning = nIsGenericDesktopRunning
    
    
    
nStopGenericDesktop :: DesktopT GenericDesktop ()
nStopGenericDesktop 
    = do
        (GenericDesktop cmd _ hdl) <- get
        if isJust hdl
            then io $ terminateProcess $ fromJust hdl
            else debugM $ cmd ++ " is not running."

    
nStartGenericDesktop :: DesktopT GenericDesktop Bool 
nStartGenericDesktop
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

nIsGenericDesktopRunning :: DesktopT GenericDesktop Bool
nIsGenericDesktopRunning
    = do
        (GenericDesktop _ _ hdl) <- get
        return $ isJust hdl







instance StartStop GenericDesktop where
    start = startGenericDesktop
    stop  = stopGenericDesktop

instance Requires GenericDesktop

startGenericDesktop :: GenericDesktop -> Moonbase GenericDesktop
startGenericDesktop
    (GenericDesktop cmd args _) = GenericDesktop cmd args <$> spawn cmd args

stopGenericDesktop :: GenericDesktop -> Moonbase ()
stopGenericDesktop
    (GenericDesktop _ _ (Just hdl)) = io $ terminateProcess hdl
stopGenericDesktop
    (GenericDesktop n _ _)          = warnM $ "desktop: " ++ n ++ " is not running but should be stopped"


newGenericDesktop :: String -> [String] -> Desktop
newGenericDesktop 
    cmd args = Desktop cmd $ GenericDesktop cmd args Nothing



justBackgroundColor :: String -> Desktop
justBackgroundColor color = newGenericDesktop "xsetroot" ["-solid", color]
