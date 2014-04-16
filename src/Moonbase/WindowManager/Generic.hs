module Moonbase.WindowManager.Generic where

import Control.Applicative
import System.Process

import Moonbase.Core
import Moonbase.Util.Application

data GenericWM = GenericWM String [String] (Maybe ProcessHandle)

instance WindowManagerClass GenericWM where
    startWM (GenericWM w args _) = GenericWM w args . Just <$> spawn w args
    stopWM (GenericWM _ _ (Just hdl)) = io $ terminateProcess hdl
    stopWM GenericWM {}               = return ()



