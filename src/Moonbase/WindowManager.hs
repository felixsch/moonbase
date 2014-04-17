module Moonbase.WindowManager where


import Control.Monad.Reader
import Control.Monad.State

import Moonbase.Core
import Moonbase.Log


startWindowManager :: Moonbase ()
startWindowManager
    = do
        infoM "Starting windowmanager..."
        st <- get
        w <- startWM . windowManager =<< ask
        put $ st { wm = Just w }

stopWindowManager :: Moonbase ()
stopWindowManager
    = infoM "Stoping windowmanager..." >> (stopWM . windowManager =<< ask)

