module Moonbase.Util.Application where

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
        check (Nothing) = throwError (ErrorMessage $ "Could not find Application `" ++ app ++ "`")
        check (Just x)  = return x 


spawn:: Application -> [Argument] -> Moonbase ProcessHandle
spawn
    app args = do
        exec <- findApp app
        io $ spawnProcess exec args

