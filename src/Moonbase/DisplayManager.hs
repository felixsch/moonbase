module Moonbase.DisplayManager
  (
  ) where

import           Control.Monad.IO.Class
import           System.Exit            (ExitCode)
import           System.Process         (readProcessWithExitCode)


data DisplayStatus = DisplayStatus
  { displayName      :: String
  , displayConnected :: Bool }


xrandr :: (MonadIO m) => [String] -> m (ExitCode, String, String)
xrandr args = liftIO $ readProcessWithExitCode "xrandr" args ""


displays :: (MonadIO m) => m [DisplayStatus]
displays = undefined








