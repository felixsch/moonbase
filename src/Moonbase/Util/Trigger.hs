module Moonbase.Util.Trigger
    ( Trigger
    , newTrigger
    , trigger
    , waitUntil
    ) where

import Control.Monad
import Control.Concurrent.MVar


type Trigger = MVar Bool

newTrigger :: IO Trigger
newTrigger
    = newEmptyMVar

trigger :: Trigger -> IO () 
trigger
    t = putMVar t True

waitUntil :: Trigger -> IO ()
waitUntil 
    = void . readMVar
