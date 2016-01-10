module Moonbase.ThemeSpec where

import           Test.Hspec
import           Test.Fake

import           Moonbase.Theme

test_bold :: SpecWith ()
test_bold = describe "#bold" $
  it "marks a font as bold" $
    fontAttrs (bold sample) `shouldBe` [Bold]
  where
    sample = monospace


spec :: Spec
spec = do
  test_bold
  describe "color_" $
    it "test implementation" $
      pending
  describe "italic" $
    it "test implementation" $
      pending
  describe "size" $
    it "test implementation" $
      pending
  describe "sans" $
    it "test implementation" $
      pending
  describe "monospace" $
    it "test implementation" $
      pending
  describe "droid" $
    it "test implementation" $
      pending
  describe "droidMono" $
    it "test implementation" $
      pending
  describe "fg" $
    it "test implementation" $
      pending
  describe "bg" $
    it "test implementation" $
      pending
  describe "font" $
    it "test implementation" $
      pending
  describe "defaultTheme" $
    it "test implementation" $
      pending
  describe "defaultColor" $
    it "test implementation" $
      pending
