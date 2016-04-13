module Moonbase.WindowManager where


import           Moonbase.Core

import           Control.Concurrent
import           Control.Monad.Reader
import           Graphics.X11.Types

import           XMonad                       hiding (xmonad)
import           XMonad.Actions.CopyWindow
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.GridVariants
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet              as SS
import           XMonad.Util.EZConfig



{- sample definition fuer XMonad

  withXMonad config $ mergeKeyoardLayout
    [ "M-Return" --> term []
    , "M-Backspace" --> term [...]
    , "M-k" --> focusNext
    , "M-j" --> focusPrev
    , "F1" --> spawn "firefox"
    , "F2" --> inX $ namedScratchpadAction myScratchpads "foo"
    ]
    ++ rangeKeys "M" [1..10] (\i -> switchTo i)
    ++ rangeKeys "M-S" [1..10] (\i -> moveTo i)
-}


class Workspace w where
  showWS :: w -> String

class (Monad m) => WindowManager m where
  term         :: [String] -> m ()
  spawn        :: [String] -> m ()
  spawnOrRaise :: [String] -> m ()
  focusNext    :: m ()
  focusPrev    :: m ()
  switchTo     :: (Workspace w) => w -> m ()
  moveTo       :: (Workspace w) => w -> m ()


data (WindowManager m) => KeyboardLayout m = KB [(String, m ())]

keyboard :: (WindowManager m) => [(String, m ())] -> KeyboardLayout m
keyboard = KB

data XMonad = XMonad
  { xmonadWS     :: [String]
  , xmonadThread :: ThreadId }

type XMonadM = ReaderT XMonad











