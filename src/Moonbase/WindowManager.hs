module Moonbase.WindowManager where


import Control.Monad.Reader
import Control.Monad.State

import Moonbase.Core


startWindowManager :: Moonbase ()
startWindowManager
    = do
        st <- get
        w <- startWM . windowManager =<< ask
        put $ st { wm = Just w }

stopWindowManager :: Moonbase ()
stopWindowManager
    = stopWM . windowManager =<< ask

