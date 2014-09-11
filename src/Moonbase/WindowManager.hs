module Moonbase.WindowManager where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except (throwError)

import Moonbase.Core
import Moonbase.Log


startWindowManager :: Moonbase ()
startWindowManager = do
        (WindowManager n _ st) <- wm <$> askConf
        infoM $ "Starting windowmanager: " ++ n
      
        ref <- newRef st

        status <- runComponentM n ref start
        
        if status 
          then modify (\x -> x { stWm = Just (RefWrapper ref) })
          else errorM ("Starting windowmanager: " ++ n ++ " failed!") >> throwError (FatalError "Windowmanager failed")

stopWindowManager :: Moonbase ()
stopWindowManager = do
    mwm <- stWm <$> get
    (WindowManager n _ _) <- wm <$> askConf
    maybe (errorM "Tried to stop the windowmanager. But it was not started..") (\(RefWrapper ref) -> void $ runComponentM n ref stop) mwm
