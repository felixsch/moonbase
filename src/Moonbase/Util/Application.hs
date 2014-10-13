module Moonbase.Util.Application 
    ( Application(..)
    , Argument
    , app
    , appWith
    , Executable(..)
    , findExecPath
    , spawn
    ) where

import Control.Monad.Except
import System.FilePath.Posix
import System.Environment.XDG.BaseDir
import System.Environment.XDG.DesktopEntry hiding (Application)
import System.Environment.XDG.MimeApps
import System.Process (spawnProcess, ProcessHandle)
import System.Directory (findExecutable)

data Application = Application String [String]

type Argument = String

app :: String -> Application
app exec = Application exec []

appWith :: String -> [Argument] -> Application
appWith = Application

class Executable a where
    execGetName     :: a -> String
    execGetPath :: a -> FilePath
    exec        :: a -> IO ()

instance Executable DesktopEntry where
    execGetName d = getName d
    execGetPath d = case getExec d of
                         Just e  -> e
                         Nothing -> getName d
    exec d        = void $ execEntry d

instance Executable Application where
    execGetName (Application app _) = takeBaseName app
    execGetPath (Application app _) = app
    exec app                        = void $ spawn app



findExecPath :: (MonadIO m) => Application -> m (Maybe FilePath)
findExecPath (Application app _) = liftIO $ findExecutable app


spawn :: (MonadIO m) =>  Application -> m (Maybe ProcessHandle)
spawn a@(Application app args) = do
        exec <- findExecPath a
        case exec of 
         Nothing -> return Nothing
         Just e -> do
                    hdl <- liftIO $ spawnProcess e args
                    return $ Just hdl

