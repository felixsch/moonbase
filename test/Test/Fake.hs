{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Test.Fake
  ( FakeMoon(..)
  , Base(..)
  , MoonTest(..)
  , FMT
  , FakeMB
  , evalTest, newEvalTest
  , fake, fakeWith
  , allowContent, allowAction
  -- Expectations
  , outputIs, outputMatches
  , hasForked, hasSetupTimeout
  -- reexports
  , get
  ) where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Identity
import           Data.List
import qualified Data.Map                as M
import qualified Data.Vector             as V
import qualified System.Timeout          as T

import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.HUnit.Lang

import           Moonbase.Core
import           Moonbase.Theme


type FakeDBus = Int
type FakeBase = Int

data FakeMoon m = FakeMoon
  { _output         :: V.Vector String
  , _allowedContent :: M.Map FilePath String
  , _forkCount      :: Int
  , _timeoutCount   :: Int
  , _loggedMessages :: V.Vector Message
  , _allActions     :: M.Map String Bool
  , _allowedActions :: M.Map String (Action (FakeMoon m) m)
  , _requireTheme   :: Bool }

emptyFakeMoon :: FakeMoon m
emptyFakeMoon = FakeMoon
  { _output         = V.empty
  , _allowedContent = M.empty
  , _forkCount      = 0
  , _timeoutCount   = 0
  , _loggedMessages = V.empty
  , _allActions     = M.empty
  , _allowedActions = M.empty
  , _requireTheme   = False }

makeLenses ''FakeMoon

newtype MoonTest a = MoonTest (StateT (FakeMoon MoonTest) IO a)
  deriving (Functor, Monad, MonadState (FakeMoon MoonTest))

type FMT = FakeMoon MoonTest
type FakeMB a = MB FMT MoonTest a

instance Applicative MoonTest where
  pure  = return
  (<*>) = ap

instance MonadState FMT (MB FMT MoonTest) where
  get = moon get
  put = moon . put

evalTest :: FMT -> MoonTest a -> IO (a, FMT)
evalTest st (MoonTest f) = runStateT f st

newEvalTest :: MoonTest a -> IO (a, FMT)
newEvalTest (MoonTest f) = runStateT f emptyFakeMoon

runFakeMB :: FakeMB a -> MoonTest a
runFakeMB f = do
  st <- get
  eval (FakeBase st) f

fakeWith :: FakeMB a -> IO (a, FMT)
fakeWith = newEvalTest . runFakeMB

fake :: FakeMB a -> IO a
fake f = fst <$> fakeWith f

allowContent :: FilePath -> String -> FakeMB ()
allowContent path content = allowedContent . at path ?= content

allowAction :: String -> Action FMT MoonTest -> FakeMB ()
allowAction name action = allowedActions . at name ?= action

-- Moonbase Implementation ----------------------------------------------------
instance Moon MoonTest where
  io       = MoonTest . lift
  puts str = output %= (|> str)
  content  = selectContent
  fork     = forkTestMoon
  delay    = io . threadDelay
  timeout  = timeoutTestMoon

instance Moonbase FMT MoonTest where
  data Base FMT = FakeBase FMT
  log msg = loggedMessages %= (|> msg)
  theme   = requireTheme .= True >> return defaultTheme
  verbose = return False
  add n _ = allActions . at n ?= True
  actions = use allowedActions

selectContent :: FilePath -> MoonTest String
selectContent path = do
  ctnt <- use allowedContent
  case ctnt ^? ix path of
    Just c  -> return c
    Nothing -> error (unlines notAllowedContent)
  where
    notAllowedContent = [ "*** Unstubbed content requested ***"
                        , "requested path:" ++ show path
                        , "to stub this output use: allowContent <path> <content>" ]

forkTestMoon :: MoonTest () -> MoonTest ThreadId
forkTestMoon f = do
  rt <- get
  forkCount += 1
  io $ forkIO (void $ evalTest rt f)

timeoutTestMoon :: Int -> MoonTest a -> MoonTest (Maybe a)
timeoutTestMoon ms f = do
  rt <- get
  timeoutCount += 1
  io $ T.timeout ms (fst <$> evalTest rt f)

-- Expectations -----------------------------------------------------------
outputIs :: FakeMB a -> [String] -> Expectation
outputIs f t = do
  (_, rt) <- fakeWith f
  V.toList (rt ^. output) `shouldBe` t

outputMatches :: FakeMB a -> String -> Expectation
outputMatches f t = do
  (_, rt) <- fakeWith f
  case V.find (isInfixOf t) (rt ^. output) of
    Just _ -> return ()
    Nothing -> assertFailure (notFound $ rt ^. output)
  where
    notFound output = unlines $ [ "Could not match `" ++ t ++ "`"
                                , "Output is:" ] ++ V.toList (formatOutput output)
    formatOutput = V.imap (\i l -> show i ++ ": " ++ l)

hasForked :: FakeMB a -> Int -> Expectation
hasForked f times = do
  (_, rt) <- fakeWith f
  rt ^. forkCount `shouldBe` times

hasSetupTimeout :: FakeMB a -> Int -> Expectation
hasSetupTimeout f times = do
  (_, rt) <- fakeWith f
  rt ^. timeoutCount `shouldBe` times
