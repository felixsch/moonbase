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
import qualified Control.Exception             as E
import           Control.Lens                  hiding (argument, (<.>))
import           Control.Monad.State
import           Control.Monad.STM             (atomically)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified DBus
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
moonbaseDir = (</>) <$> getHomeDirectory <*> pure ".moonbase"

setupHomeDirectory :: IO ()
setupHomeDirectory = do
  dir <- moonbaseDir
  exists <- doesDirectoryExist dir
  putStrLn $ "directory       : " ++ dir
  putStrLn $ "directory exists: " ++ show exists
  unless exists $ do
    putStrLn "Home directory does not exist. Creting ~/.moonbase"
    createDirectory dir

openLog :: IO Handle
openLog = do
 dir    <- moonbaseDir
 exists <- doesFileExist (dir </> "moonbase" <.> "log")

 unless exists $ writeFile (dir </> "moonbase" <.> "log") ""
 openFile (dir </> "moonbase" <.> "log") WriteMode

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
data Runtime = Runtime
  { _rtHdl      :: Handle
  , _rtActions  :: M.Map String (Action Runtime IO)
  , _rtTheme    :: Theme
  , _rtVerbose  :: Bool
  , _rtTerminal :: [String] -> MB Runtime IO ()
  , _rtQuit     :: TMVar ExitCode
  , _rtDBus     :: DBus.Client }

makeLenses ''Runtime

newRuntime :: Bool -> Handle -> TMVar ExitCode -> DBus.Client -> Runtime
newRuntime verbose handle trigger client = Runtime
  { _rtHdl      = handle
  , _rtActions  = M.empty
  , _rtTheme    = defaultTheme
  , _rtVerbose  = verbose
  , _rtTerminal = defaultTerminal
  , _rtQuit     = trigger
  , _rtDBus     = client
  }


instance MonadState Runtime (MB Runtime IO) where
  get = io . readTVarIO =<< unbase <$> ask
  put rt = do
    ref <- unbase <$> ask
    io $ atomically $ writeTVar ref rt

instance Moonbase Runtime IO where
  data Base Runtime = RWBase (TVar Runtime)

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
  verbose         = use rtVerbose
  quit exitCode   = do
    ref <- use rtQuit
    io $ atomically $ putTMVar ref exitCode

unbase :: Base Runtime -> TVar Runtime
unbase (RWBase rt) = rt

-- Com -------------------------------------------------------------------------

instance Com Runtime IO where
  call     = dbusCall
  call_    = dbusCall_
  on       = dbusOn
  callback = dbusCallback

dbusCall :: Call -> [Variant] -> MB Runtime IO [Variant]
dbusCall call args = do
    client <- use rtDBus
    reply <- io $ E.catch (DBus.call_ client method) $ \e ->
      E.throw (DBusError (DBus.clientErrorMessage e))
    return $ DBus.methodReturnBody reply
    where
      method = (DBus.methodCall path interface member) {
        DBus.methodCallBody = args
        -- FIXME: Add destination
      }
      (path, interface, member) = call

dbusCall_ :: Call -> [Variant] -> MB Runtime IO ()
dbusCall_ c args = void $ dbusCall c args

dbusOn :: (Nameable a) => a -> Help -> Usage -> ([String] -> MB Runtime IO String) -> MB Runtime IO ()
dbusOn name help usage f = do
    add key' $ Action name' help usage ActionCommand f
    ref     <- ask
    client  <- use rtDBus
    actions <- use rtActions

    io $ DBus.export client (withObjectPath "Action") $
      map (actionToMethod ref) $ M.elems actions
  where
    (name', key')                          = prepareName name
    actionToMethod ref (Action name _ _ _ f) =
      DBus.autoMethod (withInterface "Action") (DBus.memberName_ name) (wrap1 ref f)

wrap1 :: (DBus.IsValue a0) => Base Runtime -> (a0 -> MB Runtime IO b) -> a0 -> IO b
wrap1 ref f arg0 = eval ref (f arg0)

dbusCallback :: (Signal -> MB Runtime IO ()) -> MB Runtime IO ()
dbusCallback = undefined

-- basic actions for moonbase --------------------------------------------------

basicActions :: MB Runtime IO ()
basicActions = do
  on "quit"
     "Quit moonbase"
     ""
     $ \[] -> do
      trigger <- use rtQuit
      io $ atomically $ putTMVar trigger ExitSuccess
      return "Bye Bye"

  on "commands"
     "show all available commands"
     ""
     $ \_ -> do
       actions <- M.elems <$> use rtActions
       return $ unlines $
         [ "Commands available:"
         , "" ] ++ concatMap (\(Action name help usage _ _) ->
           [ "moonbase " ++ name ++ " " ++ usage
           , "  " ++ help
           , "" ]) actions

  on "run-action"
     "run a action in moonbase"
     "<name> <arg> <arg>..."
     $ \(name:args) -> do
      actions <- use rtActions
      case actions ^? ix name of
        Just (Action _ _ _ _ f)  -> f args
        otherwise              -> return "Command not found"

  on "terminal"
     "spawn a terminal"
     "[cmd] [arg arg2...]"
     $ \args -> do
      term <- use rtTerminal
      term args
      return ""

-- moonbase --------------------------------------------------------------------

moonbase :: MB Runtime IO () -> IO ()
moonbase moon = Dy.wrapMain params (Nothing, moon)
  where
    params = Dy.defaultParams {
      Dy.projectName = "moonbase"
    , Dy.realMain    = realMoonbase
    , Dy.showError   = \st msg -> st & _1 .~ Just msg
    , Dy.ghcOpts     = ["-threaded", "-Wall"]
    , Dy.includeCurrentDirectory = True }

realMoonbase :: (Maybe String, MB Runtime IO ()) -> IO ()
realMoonbase (Just err, _) = die err
realMoonbase (Nothing, runConf)  = runCli $ \verbose -> do
   setupHomeDirectory
   client <- connectDBus moonbaseBusName
   handle <- openLog
   trigger <- newEmptyTMVarIO

   when (isNothing client) $ die "Connection to DBus failed. Is moonbase already running?"

   runtime <- newTVarIO (newRuntime verbose handle trigger (fromJust client))

   eval (RWBase runtime) $ do
     basicActionsBool
     puts "Moonbase started..."
     runConf
     exitCode <- io $ atomically $ takeTMVar trigger
     when (exitCode == ExitSuccess) $ puts "Moonbase shutdown..."
     io $ exitWith exitCode

