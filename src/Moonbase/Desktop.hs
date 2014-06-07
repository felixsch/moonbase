module Moonbase.Desktop 
    ( startDesktop
    , stopDesktop
    , justBackgroundColor
    , GenericDesktop(..)
    , newGenericDesktop
    ) where

import Control.Applicative

import Control.Monad.State
import Control.Monad.Error (throwError)


import Moonbase.Core
import Moonbase.Log
import Moonbase.Desktop.Generic


handleDesktopError :: DesktopError -> Moonbase ()
handleDesktopError (DesktopError msg) = throwError $ FatalError msg



startDesktop :: Moonbase ()
startDesktop
    = do
        (Desktop n st) <- desk <$> askConf
        infoM $ "Starting Desktop " ++ n ++ "..."
        sta <- runDesktopT start st
        case sta of
            Left err -> handleDesktopError err
            Right (started, nst) -> if started
                then warnM "Desktop is allready running."
                else modify (\x -> x { stDesktop = Just $ Desktop n nst })

stopDesktop :: Moonbase ()
stopDesktop
    = maybe (warnM "Desktop is not started.")
            (\(Desktop _ st) -> void $ runDesktopT stop st) =<< stDesktop <$> get
