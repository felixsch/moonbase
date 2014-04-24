module Moonbase.WindowManager where


import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import Moonbase.Core
import Moonbase.Log


startWindowManager :: Moonbase ()
startWindowManager
    = do
        infoM "Starting windowmanager..."
        st <- get
        w <- start . windowManager =<< ask
        put $ st { wm = Just w }

stopWindowManager :: Moonbase ()
stopWindowManager
    = infoM "Stoping windowmanager..." >> (stop . fromJust . wm =<< get)

