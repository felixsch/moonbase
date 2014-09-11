module Moonbase.Desktop 
    ( startDesktop
    , stopDesktop
    , justBackgroundColor
    , GenericDesktop(..)
    , newGenericDesktop
    ) where

import Control.Applicative

import Control.Monad.State


import Moonbase.Core
import Moonbase.Log

import Moonbase.Desktop.Generic

startDesktop :: Moonbase ()
startDesktop = do
        (Desktop n _ st) <- desk <$> askConf
        ref              <- newRef st

        infoM $ "Starting Desktop " ++ n ++ "..."
        status <- runComponentM n ref start
        if status
            then modify (\x -> x { stDesktop = Just (RefWrapper ref)})
            else warnM $ "Starting desktop: " ++ n ++ " failed!"

stopDesktop :: Moonbase ()
stopDesktop = do
    maybeDesktop <- stDesktop <$> get
    (Desktop n _ _) <- desk <$> askConf
    maybe (errorM "Tried to stop the desktop. But it was not started..") (\(RefWrapper ref) -> void $ runComponentM n ref stop) maybeDesktop
