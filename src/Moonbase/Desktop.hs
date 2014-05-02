module Moonbase.Desktop 
    ( startDesktop
    , stopDesktop
    , justBackgroundColor
    , GenericDesktop(..)
    , newGenericDesktop
    ) where


import Control.Monad.State

import Data.Maybe

import Moonbase.Core
import Moonbase.Log
import Moonbase.Desktop.Generic

startDesktop :: Moonbase ()
startDesktop
    = do
        infoM "Starting desktop..."
        st <- get
        d <- start . desktop =<< askConf
        put $ st { desk = Just d }

stopDesktop :: Moonbase ()
stopDesktop
    = infoM "Stoping desktop..." >> (stop . fromJust . desk =<< get)


