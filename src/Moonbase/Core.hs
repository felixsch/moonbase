{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Moonbase.Core where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map                            as M
import           Data.Monoid

import           DBus.Client

import           Moonbase.Theme





