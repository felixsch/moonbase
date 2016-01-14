module Moonbase.CoreSpec where

import           Control.Lens
import           Control.Monad.Reader

import qualified Data.Vector as V

import           Test.Fake
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success, output)

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
  it "shows the correct show string for each message" $ do
    map show testTypes `shouldBe` testStrings
    show ActionCommand `shouldBe` "ActionCommand"
    showsPrec 1 ActionCommand "foo" `shouldBe` "ActionCommandfoo"
    showList [ActionCommand, ActionRaw] "" `shouldBe` "[ActionCommand,ActionRaw]"

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
test_mb = describe "MB" $ do
  it "checks that MB type is a valid Functor instance" $ do
     -- laws
    fmap id sample1 `isSameAs` id sample1
    fmap ((+1) . (+1)) sample1 `isSameAs` (fmap (+1) . fmap (+1)) sample1
    -- functions
    (Just <$ puts "test" <*> return 42) `isSameAs` (Just `defaultImpl` puts "test" <*> return 42)

  it "checks that MB is a valid Monad" $ do
    -- laws
    (return 1 >>= sampleComp)   `isSameAs` sampleComp 1
    (sampleComp >>= return $ 1) `isSameAs` sampleComp 1
    (return 1 >>= (\x -> sampleComp x >>= sampleComp2)) `isSameAs` ((return 1 >>= sampleComp) >>= sampleComp2)
    -- functions
    -- (>>)
    (puts "test" >> return 42) `computes` 42
    -- fail
    fake (fail "foo" >> return 1) `shouldThrow` anyIOException

  it "checks if MB has a valid MonadReader instance" $ do
    -- functions
    (_forkCount . unbase <$> ask) `computes` 0
    let setCount (FakeBase r) = FakeBase $ r { _forkCount = 42}
    local setCount (use forkCount) `computes` 0

    (puts "test" >> reader (\(FakeBase r) -> _output r)) `computes` V.fromList []

  it "checks if the Applicative instance is correct" $ do
    -- TODO: Obey the laws!
    pure 42 `isSameAs` return 42

  it
  where
    sample1 :: (Moonbase rt m) => MB rt m Int
    sample1 = return 1

    sampleComp :: (Moonbase rt m) => Int -> MB rt m Int
    sampleComp = return . (1 +)

    sampleComp2 :: (Moonbase rt m) => Int -> MB rt m Int
    sampleComp2 = return . (2 +)

    defaultImpl :: (Functor f) => a -> f b -> f a
    defaultImpl = fmap . const



spec :: Spec
spec = do
  test_exception
  test_message
  test_action
  test_actiontype
  test_moon
  test_mb
  test_eval
