module Moonbase
  ( Moonbase(..)
  ) where

import Control.Monad.Reader
import Control.Concurrent.STM.TVar


data Runtime

newtype Moonbase a = Moonbase { runMoonbase :: ReaderT (TVar Runtime) IO a }

