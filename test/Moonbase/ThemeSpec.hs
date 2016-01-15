module Moonbase.ThemeSpec where

import           Test.Hspec
import           Test.Fake

import           Moonbase.Theme

testStyle = Style "#fff" sans "#ccc"
testBg    = "#242424"
testTheme = Theme { normal    = Style "#ffffff" sans          testBg
                  , highlight = Style "#268BD2" (bold sans)   testBg
                  , active    = Style "#9ec400" sans          testBg
                  , disabled  = Style "#808080" (italic sans) testBg
                  , frame     = Style "#151515" sans          testBg }
testFont  = Font "Monospace" 12 [Italic]

spec :: Spec
spec = do
  describe "#color_" $ do
    it "should return the same color definition if the given is correct" $ do
      color_ "#fff" `shouldBe` "#ffffff"
      color_ "#aaff00" `shouldBe` "#aaff00"
    it "should return magenta if a invalid color was set" $ do
      color_ "#ff" `shouldBe` "#ff00ff"
      color_ "#aabbxx" `shouldBe` "#ff00ff"

  describe "FontAttr" $ do
    it "shows that FontAttr has a correct Eq instance" $ do
      Bold == Bold `shouldBe` True
      Italic /= Bold `shouldBe` True

  describe "Font" $ do
    it "checks if the records setup correctly" $ do
      fontName  testFont `shouldBe` "Monospace"
      fontSize  testFont `shouldBe` 12
      fontAttrs testFont `shouldBe` [Italic]
    it "checks if Eq works" $ do
      testFont == testFont `shouldBe` True
      testFont /= Font "" 8 [] `shouldBe` True
    it "checks if Show works correctly" $ do
      show testFont `shouldBe` "Font {fontName = \"Monospace\", fontSize = 12, fontAttrs = [Italic]}"
      showsPrec 1 testFont "foo" `shouldBe` "Font {fontName = \"Monospace\", fontSize = 12, fontAttrs = [Italic]}foo"
      showList [testFont] "" `shouldBe` "[Font {fontName = \"Monospace\", fontSize = 12, fontAttrs = [Italic]}]"

  describe "#italic" $
    it "adds the italic Attribute to a font" $
      italic (Font "Monospace" 12 []) `shouldBe` Font "Monospace" 12 [Italic]

  describe "#bold" $
    it "adds the bold Attribute to a font" $
      bold (Font "Monospace" 12 []) `shouldBe` Font "Monospace" 12 [Bold]

  describe "#size" $
    it "sets the size of a font" $
      size 42 (Font "Monospace" 12 []) `shouldBe` Font "Monospace" 42 []

  describe "all default font definitions" $
    it "checks the default font definitions" $ do
      sans `shouldBe` Font "Sans" 12 []
      monospace `shouldBe` Font "Monospace" 12 []
      droid `shouldBe` Font "Droid Sans" 12 []
      droidMono `shouldBe` Font "Droid Sans Mono" 12 []

  describe "Style" $ do
    it "checks if Style has a valid Eq instance" $ do
      testStyle == testStyle `shouldBe` True
      testStyle /= testStyle `shouldBe` False
    it "checks if Style has a valid Show instance" $ do
      show testStyle `shouldBe` "Style \"#fff\" (Font {fontName = \"Sans\", fontSize = 12, fontAttrs = []}) \"#ccc\""
      showsPrec 1 testStyle "foo" `shouldBe` "Style \"#fff\" (Font {fontName = \"Sans\", fontSize = 12, fontAttrs = []}) \"#ccc\"foo"
      showList [testStyle] "" `shouldBe` "[Style \"#fff\" (Font {fontName = \"Sans\", fontSize = 12, fontAttrs = []}) \"#ccc\"]"


  describe "#fg" $
    it "selects the foreground color of a style" $
      fg testStyle `shouldBe` "#fff"

  describe "#bg" $
    it "selects the background color of a style" $
      bg testStyle `shouldBe` "#ccc"

  describe "#font" $
    it "selects the background color of a style" $
      font testStyle `shouldBe` sans

  describe "#defaultColor" $
    it "the default color should be magenta" $
      defaultColor `shouldBe` "#ff00ff"

  describe "Theme" $ do
    it "checks if records are setup correctly" $ do
      fg (normal testTheme) `shouldBe` "#ffffff"
      fg (highlight testTheme) `shouldBe` "#268BD2"
      fg (active testTheme) `shouldBe` "#9ec400"
      fg (disabled testTheme) `shouldBe` "#808080"
      fg (frame testTheme) `shouldBe` "#151515"
    it "shows that Theme has a valid Eq instance" $ do
      testTheme == testTheme `shouldBe` True
      testTheme /= defaultTheme `shouldBe` False

  describe "defaultTheme" $
    it "checks for a sane default theme" $
      defaultTheme `shouldBe` testTheme
