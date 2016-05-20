{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Moonbase.WindowManager.XMonad
  ( XMonad
  , withXMonad
  , setLayout
  , setWorkspaces
  ) where


import           Moonbase.Core
import           Moonbase.Workspace

import           Moonbase.WindowManager
import           Moonbase.WindowManager.XMonad.Impl

import           Control.Concurrent
import           Control.Monad.State
import           Graphics.X11.Types
import           System.Environment

import           XMonad                             hiding (io, setLayout,
                                                     xmonad)
import           XMonad.Actions.CopyWindow
import           XMonad.Config
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.GridVariants
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet                    as SS
import           XMonad.Util.EZConfig

data XMonad = forall (l :: * -> *).
                      ( Read (l Window),
                        LayoutClass l Window ) => XMonad
  { xmonadCfg    :: XConfig l
  , xmonadThread :: Maybe ThreadId }


setLayoutRuntime :: (Read (l Window), LayoutClass l Window) => l Window -> XMonad -> XMonad
setLayoutRuntime layout XMonad{..} = XMonad{xmonadCfg = set xmonadCfg, xmonadThread = Nothing}
    where
    set XConfig{..} = XConfig{layoutHook = layout, ..}

setWorkspacesRuntime :: (Workspace w) => [w] -> XMonad -> XMonad
setWorkspacesRuntime ws XMonad{..} = XMonad{xmonadCfg = set xmonadCfg, xmonadThread = Nothing}
    where
    set XConfig{..} = XConfig{workspaces = map workspaceName ws, ..}


defaultXMonadRuntime :: XMonad
defaultXMonadRuntime = XMonad defaultConfig Nothing

type XMonadM rt m = StateT XMonad (MB rt m)


runXMonadM :: (Moonbase rt m) => XMonadM rt m () -> MB rt m XMonad
runXMonadM f = snd <$> runStateT f defaultXMonadRuntime


instance (Moonbase rt m) => WindowManager (XMonadM rt m) where
  setKeyboardLayout _ = return ()


withXMonad :: (Moonbase rt m, Workspace w) => [w] -> XMonadM rt m () -> MB rt m XMonad
withXMonad ws conf = do
  runtime <- runXMonadM configure
  thread  <- fork $ io $ runXMonad runtime

  return $ runtime { xmonadThread = Just thread }
  where
    runXMonad XMonad{ xmonadCfg = cfg } = moonbaseXMonad cfg
    configure = setWorkspaces ws >> conf

setLayout :: (Moonbase rt m, Read (l Window), LayoutClass l Window) => l Window -> XMonadM rt m ()
setLayout layout = modify (setLayoutRuntime layout)

setWorkspaces :: (Moonbase rt m, Workspace w) => [w] -> XMonadM rt m ()
setWorkspaces ws = modify (setWorkspacesRuntime ws)
