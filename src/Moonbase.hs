 module Moonbase
  ( moonbase
  , module Moonbase.Core
  , module Moonbase.DBus
  , module Moonbase.Theme
  ) where

import           Control.Applicative
import           Control.Lens                   hiding (argument, (<.>))
import           Control.Monad
import           Control.Monad.STM              (atomically)
import qualified Config.Dyre                    as Dy
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent             (forkOS)

import           System.Directory
import           System.FilePath.Posix
import           System.IO

import qualified Data.Map                       as M
import           Data.Time.Format
import           Data.Time.LocalTime

import           DBus                           hiding (Signal, signal)
import           DBus.Client


import           Moonbase.Core
import           Moonbase.DBus
import           Moonbase.Cli
import           Moonbase.Theme

moonbase :: IO ()
moonbase = putStrLn "do nothing"
