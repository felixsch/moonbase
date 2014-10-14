{-|
Module      : Moonbase.Util.Application
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX

generic implementations of fork and timeout

-}

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

-- | generic fork
fork :: (MonadBaseControl IO m) => m () -> m ThreadId
fork = liftBaseDiscard forkIO


-- | generic timeout
timeout :: (MonadBaseControl IO m) => Int -> m () -> m ()
timeout t m = void $ liftBaseWith (\runInIO -> T.timeout t (runInIO m))


