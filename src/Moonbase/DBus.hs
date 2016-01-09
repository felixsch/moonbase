{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Moonbase.DBus where

import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import           DBus
import           DBus.Client

import           Data.Char                   (toLower, toUpper)
import qualified Data.Map                    as M
import           Data.Maybe

import           Moonbase.Core
