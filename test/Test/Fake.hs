{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Test.Fake where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified Data.Map                as M
import qualified Data.Vector             as V
import qualified System.Timeout          as T

import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.HUnit.Lang

import           Moonbase.Core


type FakeDBus = Int
type FakeBase = Int

data FakeMoon = FakeMoon
  { _output         :: V.Vector String
  , _allowedContent :: M.Map FilePath String
  , _forkCount      :: Int
  , _timeoutCount   :: Int
  , _baseCount      :: Int
  , _log            :: V.Vector Message
  , _allActions     :: M.Map String Bool
  , _requireTheme   :: Bool
  , _dbusStub       :: FakeDBus }

emptyFakeMoon :: FakeMoon
emptyFakeMoon = FakeMoon
  { _output         = V.empty
  , _allowedContent = M.empty
  , _forkCount      = 0
  , _timeoutCount   = 0
  , _baseCount      = 0
  , _log            = V.empty
  , _allActions     = M.empty
  , _requireTheme   = False
  , _dbusStub       = 0 }

makeLenses ''FakeMoon

newtype MoonTest a = MoonTest (StateT FakeMoon IO a)
  deriving (Functor, Monad, MonadState FakeMoon)

instance Applicative MoonTest where
  pure  = return
  (<*>) = ap

evalTest :: FakeMoon -> MoonTest a -> IO (a, FakeMoon)
evalTest st (MoonTest f) = runStateT f st

newEvalTest :: MoonTest a -> IO (a, FakeMoon)
newEvalTest (MoonTest f) = runStateT f emptyFakeMoon

fake :: MoonTest a -> IO a
fake f = fst <$> newEvalTest f

allowContent :: FilePath -> String -> MoonTest ()
allowContent path content = allowedContent . at path ?= content

-- Fake Moon Implementation ----------------------------------------------------
instance Moon MoonTest where
  io       = MoonTest . lift
  puts str = output %= (|> str)
  content  = selectContent
  fork     = forkTestMoon
  timeout  = timeoutTestMoon

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

-- Moon Expectations -----------------------------------------------------------
outputIs :: MoonTest a -> [String] -> Expectation
outputIs f t = do
  (_, rt) <- newEvalTest f
  V.toList (rt ^. output) `shouldBe` t

outputMatches :: MoonTest a -> String -> Expectation
outputMatches f t = do
  (_, rt) <- newEvalTest f
  case V.find (isInfixOf t) (rt ^. output) of
    Just _ -> return ()
    Nothing -> assertFailure (notFound $ rt ^. output)
  where
    notFound output = unlines $ [ "Could not match `" ++ t ++ "`"
                                , "Output is:" ] ++ V.toList (formatOutput output)
    formatOutput = V.imap (\i l -> show i ++ ": " ++ l)

hasForked :: MoonTest a -> Int -> Expectation
hasForked f times = do
  (_, rt) <- newEvalTest f
  rt ^. forkCount `shouldBe` times

hasSetupTimeout :: MoonTest a -> Int -> Expectation
hasSetupTimeout f times = do
  (_, rt) <- newEvalTest f
  rt ^. timeoutCount `shouldBe` times
