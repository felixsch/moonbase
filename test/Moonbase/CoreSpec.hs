module Moonbase.CoreSpec where

import           Test.Fake
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success)
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

test_message :: SpecWith ()
test_message = describe "Message" $
  it "shows the correct show string for each message" $
    map show testMessages `shouldBe` testStrings
  where
    testMessages = [ Warning "warning"
                   , Info "info"
                   , Success "success"
                   , Debug "debug" ]
    testStrings  = [ "Warning: warning"
                   , "Info: info"
                   , "Success: success"
                   , "Debug: debug" ]

instance Arbitrary ActionType where
  arbitrary = do
    i <- choose (0, 2) :: Gen Int
    return $ [ActionCommand, ActionFunction, ActionRaw] !! i

test_actiontype :: SpecWith ()
test_actiontype = describe "ActionType" $ do
  it "shows the correct show string for each message" $
    map show testTypes `shouldBe` testStrings
  prop "tests the equality of ActionTypes" testEq
  where
    testEq :: ActionType -> ActionType -> Bool
    testEq a b = (a == b) /= (a /= b)

    testTypes   = [ ActionCommand, ActionFunction, ActionRaw ]
    testStrings = [ "ActionCommand", "ActionFunction", "ActionRaw" ]

test_action :: SpecWith ()
test_action = describe "Action" $ do
  it "test all records fields of a Action" $ do
    _actionName a `shouldBe` "foo"
    _actionHelp a `shouldBe` "bar"
    _actionType a `shouldBe` ActionRaw
  it "test the action record" $
    fake (_action a []) `shouldReturn` "test"

  where
    a = Action { _actionName = "foo"
               , _actionHelp = "bar"
               , _actionType = ActionRaw
               , _action     = actionF }
    actionF :: [String] -> FakeMB String
    actionF _ = return "test"


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


test_mb :: SpecWith ()
test_mb = describe "MB" $
  it "checks that MB type is a valid Functor instance" $ do
    fmap id sample1 `isSameAs` id sample1
    fmap ((+1) . (+1)) sample1 `isSameAs` (fmap (+1) . fmap (+1)) sample1

  where
    sample1 :: (Moonbase rt m) => MB rt m Int
    sample1 = return 1




spec :: Spec
spec = do
  test_exception
  test_message
  test_action
  test_actiontype
  test_moon
  test_mb
  test_eval

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
