module Moonbase.CoreSpec where

import           Test.Fake
import           Test.Hspec

import           Moonbase.Core

test_exception :: SpecWith ()
test_exception = describe "Exceptions" $
  it "shows the correct show string for each exception" $
    map show testExceptions `shouldBe` testStrings
  where
    testExceptions = [ CouldNotOpenDisplay
                     , DBusError "Sample"
                     , FileNotFound "/sample"
                     , Shutdown ]
    testStrings = [ "Could not open display"
                  , "DBus error: Sample"
                  , "No such file or directory: /sample"
                  , "Bye!" ]

test_moon :: SpecWith ()
test_moon = describe "#moon" $
  it "checks if moon lifts a computation to Moonbase" $
    fake (moon sampleM) `shouldReturn` 42
  where
    sampleM :: MoonTest Int
    sampleM = return 42

test_eval :: SpecWith ()
test_eval = describe "#eval" $
  it "evaluates a computation in the MB Monad" $
    runInMoonTest (\st -> eval (FakeBase st) (sampleM 1 >>= sampleM)) `shouldReturn` 3
  where
    sampleM :: Int -> FakeMB Int
    sampleM x = return $ x + 1

    runInMoonTest :: (FMT -> MoonTest a) -> IO a
    runInMoonTest f = fst <$> newEvalTest (f =<< get)




spec :: Spec
spec = do
  test_exception
  test_moon

  describe "#action" $
    it "test implementation" $
      pending
  describe "#actionHelp" $
    it "test implementation" $
      pending
  describe "#actionName" $
    it "test implementation" $
      pending
  describe "#actionResult" $
    it "test implementation" $
      pending
  describe "#actionError" $
    it "test implementation" $
      pending
  describe "#actionNothing" $
    it "test implementation" $
      pending
  describe "#hdl" $
    it "test implementation" $
      pending
  describe "#actions" $
    it "test implementation" $
      pending
  describe "#theme" $
    it "test implementation" $
      pending
  describe "#dbus" $
    it "test implementation" $
      pending
  describe "#isVerbose" $
    it "test implementation" $
      pending
