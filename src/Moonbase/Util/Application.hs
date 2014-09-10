module Moonbase.Util.Application 
    ( Application
    , Argument
    , findApp
    , spawn
    ) where

import Control.Monad.Except

import System.Process (spawnProcess, ProcessHandle)
import System.Directory (findExecutable)

import Moonbase.Core
import Moonbase.Log



type Application = String
type Argument = String


findApp :: (MonadIO m) => Application -> m (Maybe FilePath)
findApp
    app = io (findExecutable app)


spawn:: (Logger m, MonadIO m) =>  Application -> [Argument] -> m (Maybe ProcessHandle)
spawn
    app args = do
        debugM $ " --: spawning " ++ app
        exec <- findApp app
        case exec of 
         Nothing -> return Nothing
         Just e -> do
                    hdl <- io $ spawnProcess e args
                    return $ Just hdl

