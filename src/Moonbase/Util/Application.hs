module Moonbase.Util.Application 
    ( Application
    , Argument
    , findApp
    , spawn
    ) where

import Control.Monad.Except

import System.Process (spawnProcess, ProcessHandle)
import System.Directory (findExecutable)

import Moonbase



type Application = String
type Argument = String


findApp :: (MonadIO m) => Application -> m (Maybe FilePath)
findApp app = liftIO $ findExecutable app


spawn :: (MonadIO m) =>  Application -> [Argument] -> m (Maybe ProcessHandle)
spawn app args = do
        exec <- findApp app
        case exec of 
         Nothing -> return Nothing
         Just e -> do
                    hdl <- liftIO $ spawnProcess e args
                    return $ Just hdl

