module Moonbase.Preferred where

import Control.Applicative

import System.IO.Error
import Control.Monad.Error
import Control.Monad.Reader

import Data.Either (either)
import qualified Data.Map as M
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Environment.XDG.DesktopEntry
import System.Environment.XDG.MimeApps

import Moonbase.Core

userMimeApps :: IO FilePath
userMimeApps
    = do
        dir <- getUserDataDir
        return $ dir ++ "/applications/mimeapps.list"

setPreferred :: Moonbase ()
setPreferred
    = set =<< io load 
    where
        load = catchIOError (loadMimeApps =<< userMimeApps) (\_ -> return newMimeApps)
        fromPreferred (AppName n) = n ++ ".desktop"
        fromPreferred (Entry e) = getName e ++ ".desktop"
        update app = M.foldlWithKey updateMime app <$> (preferred <$> ask)
        updateMime a m n = addDefault m (fromPreferred n) a
        set app = do
            dir <- io $ userMimeApps
            up <- update app
            io $ saveMimeApps dir up
    
            
