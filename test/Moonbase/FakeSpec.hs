module Moonbase.FakeSpec where

import           Test.Fake
import           Test.Hspec

import           Moonbase.Core

-- This file is here for pure testing purpose ----------------------------------


test_outputIs :: SpecWith ()
test_outputIs = describe "`outputIs`" $
  it "returns the same output as Moon based methods would do" $
    (puts "Hello" >> puts "World") `outputIs` ["Hello", "World"]

test_outputMatches :: SpecWith ()
test_outputMatches = describe "`outputMatches`" $
  it "matches the output of a Moon based method" $
    (puts "Hello Haskell is a great language" >> puts "World, I love it!") `outputMatches` "love"

test_hasForked :: SpecWith ()
test_hasForked = describe "`hasForked`" $
  it "checks if a implementation uses fork" $
    fork (return ()) `hasForked` 1

test_hasSetupTimeout :: SpecWith ()
test_hasSetupTimeout = describe "`hasSetupTimeout`" $
  it "checks if a implementation uses timeout" $
    timeout 100 (return ()) `hasSetupTimeout` 1

test_allowContent :: SpecWith ()
test_allowContent = describe "`allowContent`" $
  it "checks if a readFile could be stubbed" $
    fake (allowContent "/tmp/sample.txt" "test" >> content "/tmp/sample.txt") `shouldReturn` "test"

spec :: Spec
spec = context "Test test helper functions" $ do
    test_outputIs
    test_outputMatches
    test_hasForked
    test_hasSetupTimeout
    test_allowContent
