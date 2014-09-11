{-# LANGUAGE FlexibleContexts #-}

module Moonbase.Util.Process 
  ( fork
  , forkIO
  , timeout
  , ThreadId
  ) where

import Control.Monad                 ( void )
import Control.Monad.Trans.Control   ( MonadBaseControl , liftBaseDiscard, liftBaseWith )
import Control.Concurrent            ( forkIO, ThreadId )
import qualified System.Timeout as T ( timeout )

fork :: (MonadBaseControl IO m) => m () -> m ThreadId
fork = liftBaseDiscard forkIO


timeout :: (MonadBaseControl IO m) => Int -> m () -> m ()
timeout t m = void $ liftBaseWith (\runInIO -> T.timeout t (runInIO m))


