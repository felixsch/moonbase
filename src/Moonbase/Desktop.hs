module Moonbase.Desktop where

import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import Moonbase.Core
import Moonbase.Log

startDesktop :: Moonbase ()
startDesktop
    = do
        infoM "Starting desktop..."
        st <- get
        d <- start . desktop =<< ask
        put $ st { desk = Just d }

stopDesktop :: Moonbase ()
stopDesktop
    = infoM "Stoping desktop..." >> (stop . fromJust . desk =<< get)

