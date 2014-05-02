module Moonbase.Hook
    ( runHooks
    ) where

import Control.Applicative
import Control.Monad.State

import Moonbase.Core



runHooks :: Moonbase ()
runHooks
    = mapM_ start' =<< hooks <$> askConf
    where
        start' (Hook x) = do
            st <- get
            nh <- start x
            put $ st { hks = Hook nh : hks st}

    

