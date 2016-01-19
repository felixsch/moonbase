{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Moonbase
  ( moonbase
  , module Moonbase.Core
  , module Moonbase.DBus
  , module Moonbase.Theme
  ) where

import qualified Config.Dyre                   as Dy
import           Control.Applicative
import           Control.Concurrent            (forkIO, forkOS, threadDelay)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Lens                  hiding (argument, (<.>))
import           Control.Monad.State
import           Control.Monad.STM             (atomically)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified DBus.Client                   as DBus
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process
import qualified System.Timeout                as T


import           Moonbase.Cli
import           Moonbase.Core
import           Moonbase.DBus
import           Moonbase.Theme

-- exported stuff
import qualified Moonbase.Cli                  as Moonbase
import qualified Moonbase.Core                 as Moonbase
import qualified Moonbase.DBus                 as Moonbase
import qualified Moonbase.Theme                as Moonbase

-- Core Implementations --------------------------------------------------------

logMessage :: Handle -> Message -> IO ()
logMessage handle msg = do
  date <- formatLogDate
  hPutStrLn handle $ date ++ ": " ++ show msg
  where
    formatLogDate = formatTime defaultTimeLocale rfc822DateFormat <$> getZonedTime


-- FIXME: Until directory 1.2.3 is release, wich adds getXdgDirectory support
--        use this directory
moonbaseDir ::  IO FilePath
moonbaseDir = (</>) <$> getHomeDirectory <*> pure "moonbase"

setupHomeDirectory :: IO ()
setupHomeDirectory = do
  dir <- moonbaseDir
  exists <- doesDirectoryExist dir
  unless exists $ createDirectory dir

openLog :: IO Handle
openLog = do
 dir    <- moonbaseDir
 exists <- doesFileExist (dir </> "moonbase" <.> "log")

 unless exists $ writeFile (dir </> "moonbase" <.> "log") ""
 openFile (dir </> "moonbase" <.> "log") WriteMode

-- moonbase --------------------------------------------------------------------

moonbase :: MB (Runtime IO) IO () -> IO ()
moonbase moon = Dy.wrapMain params (Nothing, moon)
  where
    params = Dy.defaultParams {
      Dy.projectName = "moonbase"
    , Dy.realMain    = realMoonbase
    , Dy.showError   = \st msg -> st & _1 .~ Just msg
    , Dy.ghcOpts     = ["-threaded", "-Wall"]
    , Dy.includeCurrentDirectory = True }

realMoonbase :: (Maybe String, MB (Runtime IO) IO ()) -> IO ()
realMoonbase (Just err, _) = die err
realMoonbase (Nothing, f)  = runCli $ \verbose -> do
   client <- connectDBus moonbaseBusName
   handle <- openLog
   trigger <- newEmptyTMVarIO

   when (isNothing client) $ die "Connection to DBus failed. Is moonbase already running?"

-- Moon ------------------------------------------------------------------------

instance Moon IO where
  io      = id
  puts    = putStrLn
  content = readFile
  fork    = forkIO
  delay   = threadDelay
  timeout = T.timeout
  exec cmd args = readProcessWithExitCode cmd args ""
-- Moonbase / Base -------------------------------------------------------------
data Runtime m = Runtime
  { _rtHdl      :: Handle
  , _rtActions  :: M.Map String (Action (Runtime m) m)
  , _rtTheme    :: Theme
  , _rtVerbose  :: Bool
  , _rtTerminal :: [String] -> MB (Runtime m) m ()
  , _rtQuit     :: TMVar Bool
  , _rtDBus     :: DBus.Client }

makeLenses ''Runtime

newRuntime :: Bool -> Handle -> TMVar Bool -> DBus.Client -> Runtime IO
newRuntime verbose handle trigger client = Runtime
  { _rtHdl      = handle
  , _rtActions  = M.empty
  , _rtTheme    = defaultTheme
  , _rtVerbose  = verbose
  , _rtTerminal = defaultTerminal
  , _rtQuit     = trigger
  , _rtDBus     = client
  }

type RWRT = Runtime IO
type RWMB a = MB RWRT IO a

instance MonadState RWRT (MB RWRT IO) where
  get = io . readTVarIO =<< unbase <$> ask
  put rt = do
    ref <- unbase <$> ask
    io $ atomically $ writeTVar ref rt

instance Moonbase RWRT IO where
  data Base RWRT = RWBase (TVar RWRT)

  log message    = do
    handle <- use rtHdl
    verbose <- use rtVerbose
    io $ logMessage handle message
    when verbose $ puts $ show message

  theme           = use rtTheme
  withTheme t     = rtTheme .= t

  terminal args   = do
    f <- use rtTerminal
    f args

  withTerminal f  = rtTerminal .= f

  add str action  = rtActions . at str ?= action
  actions         = use rtActions

  verbose         = use rtVerbose
  quit            = do
    ref <- use rtQuit
    io $ atomically $ putTMVar ref True

unbase :: Base RWRT -> TVar RWRT
unbase (RWBase rt) = rt

-- Com -------------------------------------------------------------------------
