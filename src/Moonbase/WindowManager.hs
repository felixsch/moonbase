module Moonbase.WindowManager where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error (throwError)

import Moonbase.Core
import Moonbase.Log

handleWMError :: WMError -> Moonbase ()
handleWMError (WMError msg) = throwError $ FatalError msg

startWindowManager :: Moonbase ()
startWindowManager = do
        (WindowManager n st) <- windowManager <$> askConf
        infoM $ "Starting windowmanager: " ++ n
        
        sta <- runWindowManagerT start st

        case sta of 
            Left err            -> handleWMError err
            Right (started, nst) -> if started
                then modify (\x -> x { wm = Just $ WindowManager n nst})
                else errorM "Windowmanager starting failed..."

stopWindowManager :: Moonbase ()
stopWindowManager = do
    mwm <- wm <$> get
    maybe (errorM "Tried to stop the windowmanager. But it was not started..")
          (\(WindowManager _ st) -> void $ runWindowManagerT stop st) mwm
