{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Moonbase.Workspace
  ( Workspace(..)
  , Workspaces(..)
  , hasWorkspaces
  ) where

import           Moonbase.Core

{-

 - Naming of workspace
 - Identifier for current workspace

-}

class (Show a) => Workspace a where
  workspaceName ::  a -> String

instance Workspace Int where
  workspaceName = show

instance Workspace String where
  workspaceName = show


data Workspaces a = Workspaces [a]

hasWorkspaces :: (Moonbase rt m, Workspace a) => [a] -> MB rt m (Workspaces a)
hasWorkspaces = pure . Workspaces

