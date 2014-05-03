module Moonbase.WindowManager.Generic
    ( GenericWM(..)
    , newGenericWM
    ) where

import Control.Applicative
import System.Process

import Moonbase.Core
import Moonbase.Log
import Moonbase.Util.Application

data GenericWM = GenericWM String [String] (Maybe ProcessHandle)

instance StartStop GenericWM where
    start (GenericWM w args _) = GenericWM w args . Just <$> spawn w args
    stop (GenericWM _ _ (Just hdl)) = io $ terminateProcess hdl
    stop (GenericWM n _ _ )         = warnM $ "wm: " ++ n ++ " is not running but should be stopped"

instance Requires GenericWM

newGenericWM :: String -> [String] -> WindowManager
newGenericWM
    cmd args = WindowManager cmd $ GenericWM cmd args Nothing



