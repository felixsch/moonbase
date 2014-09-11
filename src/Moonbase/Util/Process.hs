{-# LANGUAGE FlexibleContexts #-}

module Moonbase.Util.Process 
  ( fork
  , timeout 
  ) where

import Control.Monad                 ( void )
import Control.Monad.Trans.Control   ( MonadBaseControl , liftBaseDiscard, liftBaseWith )
import Control.Monad.IO.Class        ( liftIO )
import qualified System.Timeout as T ( timeout )

fork :: (MonadBaseControl IO m) => m () -> m ()
fork = liftBaseDiscard liftIO


timeout :: (MonadBaseControl IO m) => Int -> m () -> m ()
timeout t m = void $ liftBaseWith (\runInIO -> T.timeout t (runInIO m))


