{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Moonbase.WindowManager
  ( Keybinding
  , KeyboardLayout(..)
  , WindowManager(..)
  , WMActions(..)
  ) where


import           Moonbase.Core
import           Moonbase.Workspace

type Keybinding = String


data KeyboardLayout m = KeyboardLayout [(Keybinding, m ())]


class (Monad m) => WindowManager m where
  setKeyboardLayout :: KeyboardLayout m -> m ()


class (WindowManager m) => WMActions m where
  term         :: [String] -> m ()
  spawnOrRaise :: [String] -> m ()
  focusNext    :: m ()
  focusPrev    :: m ()
  switchTo     :: (Workspace w) => w -> m ()
  moveTo       :: (Workspace w) => w -> m ()
