module Moonbase.Util.Application 
    ( Application
    , Argument
    , findApp
    , spawn
    ) where

import Control.Monad.Error

import System.Process (spawnProcess, ProcessHandle)
import System.Directory (findExecutable)

import Moonbase.Core



type Application = String
type Argument = String


findApp :: Application -> Moonbase FilePath
findApp
    app = check =<< io (findExecutable app)
    where
        check (Nothing) = throwError $ AppNotFound app
        check (Just x)  = return x 


spawn:: Application -> [Argument] -> Moonbase ProcessHandle
spawn
    app args = do
        exec <- findApp app
        io $ spawnProcess exec args

