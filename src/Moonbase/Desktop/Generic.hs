module Moonbase.Desktop.Generic
    ( GenericDesktop(..)
    , newGenericDesktop
    ) where


import Control.Monad (void)
import Control.Applicative
import System.Process

import Data.Maybe (isNothing)

import Moonbase.Core
import Moonbase.Log
import Moonbase.Util.Application

data GenericDesktop = GenericDesktop String [String] (Maybe ProcessHandle)

instance StartStop GenericDesktop where
    start = startGenericDesktop
    stop  = stopGenericDesktop

startGenericDesktop :: GenericDesktop -> Moonbase GenericDesktop
startGenericDesktop
    (GenericDesktop cmd args _) = GenericDesktop cmd args . Just <$> spawn cmd args

stopGenericDesktop :: GenericDesktop -> Moonbase ()
stopGenericDesktop
    (GenericDesktop _ _ (Just hdl)) = io $ terminateProcess hdl
stopGenericDesktop
    (GenericDesktop n _ _)          = warnM $ "desktop: " ++ n ++ " is not running but should be stopped"


newGenericDesktop :: String -> [String] -> Desktop
newGenericDesktop 
    cmd args = Desktop cmd $ GenericDesktop cmd args Nothing
