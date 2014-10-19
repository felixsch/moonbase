{-|
Module      : Moonbase.Theme
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX

To configure moonbase properly defining a basic theme is important. You can define your own theme or
use theme which are allready defined by a maintainer for you.

Example theme configuration:

> droidSans = Font
>   { fontName = "Droid Sans Mono"
>   , fontSize = 12
>   , fontAttr = [] }
> 
> myTheme = Theme
>   { bg        = "#cccccc"
>   , normal    = droidSans
>   , normalC   = "#ffffff"
>   , hl        = droidSans
>   , hlC1      = "#1cff50"
>   , hlA1      = [Bold]
>   , hlC2      = magenta
>   , hlA2      = []
>   , disabledC = "#221312"
>   , disabledA = [Italic] }
>    
> 
> main :: IO ()
> main = moonbase myConfig $ do
>     setTheme myTheme
>     ...
> 
-}

module Moonbase.Theme
  ( Theme(..)
  , defaultTheme
  , Color(..)
  , color_
  , FontAttribute(..)
  , Font(..)
  , bold
  , italic
  , underline
  , size
  , defaultSans
  , defaultMonospace
  , black, white, red, lime, blue
  , yellow, cyan, magenta, silver
  , gray, maroon, olive, green
  , purple, teal, navy
  ) where

import Control.Applicative

import Data.Char

import {-# SOURCE #-} Moonbase

-- | Color defined as html heximal color string (eg. "#fafa21")
type Color = String

-- | Simple check if the color is correct
color_ :: Color -> Color
color_ ['#', r, g, b]                = ['#', r, r, g, g, b, b]
color_ ['#', r1, r2, g1, g2, b1, b2] = case checkHex [r1, r2, g1, g2, b1, b2] of
                                            Just x  -> '#' : x
                                            Nothing -> magenta
color_ _                             = magenta

checkHex :: String -> Maybe String
checkHex []     = Just []
checkHex (x:xs) = if isHexDigit x
                     then (x :) <$> checkHex xs
                     else Nothing

-- | Font attributes
data FontAttribute = Bold      -- ^ Draw the font bold
                   | Thin      -- ^ Draw the font thiner as normal
                   | Normal    -- ^ Reset all attributes
                   | Italic    -- ^ Draw font italic
                   | Underline -- ^ Underline font

-- | Font settings 
data Font = Font
  { fontName :: String          -- ^ Name of the font (Name __must__ match the font-config name)
  , fontSize :: Int             -- ^ Size of the font
  , fontAttr :: [FontAttribute] -- ^ Attributes like bold, italic
  }

-- | make a font bold
bold :: Font -> Font
bold font = font { fontAttr = Bold : (fontAttr font) }

-- | make a font italic
italic :: Font -> Font
italic font = font { fontAttr = Italic : (fontAttr font) }

-- | underline a font
underline :: Font -> Font
underline font = font { fontAttr = Underline : (fontAttr font) }

-- | change the size
size :: Int -> Font -> Font
size s font = font { fontSize = s }


-- | Moonbase theme definition
data Theme = Theme
  { bg        :: Color             -- ^ Background color
  , normal    :: Font              -- ^ Normal font
  , normalC   :: Color             -- ^ Normal font color
  , hl        :: Font              -- ^ Highlight font
  , hlC1      :: Color             -- ^ First Highlight color
  , hlA1      :: [FontAttribute]   -- ^ First Highlight attributes
  , hlC2      :: Color             -- ^ Second Highlight color
  , hlA2      :: [FontAttribute]   -- ^ Second hightlight attributes
  , active    :: Font              -- ^ Font of active elements
  , activeC   :: Color             -- ^ Color of inactive elements
  , disabledC :: Color             -- ^ Color of disabled items
  , disabledA :: [FontAttribute]   -- ^ Attributes of disabled items
  }


-- | Default Sans font definition
defaultSans :: Font
defaultSans = Font "Sans" 12 []

-- | Default Monospace font definition
defaultMonospace :: Font
defaultMonospace = Font "Monospace" 12 []

-- | Moonbase default theme
defaultTheme :: Theme
defaultTheme = Theme
  { bg        = "#242424"
  , normal    = defaultSans
  , normalC   = white
  , active    = defaultSans
  , activeC   = "#9ec400"
  , hl        = defaultSans
  , hlC1      = lime
  , hlA1      = [Bold]
  , hlC2      = magenta
  , hlA2      = []
  , disabledC = gray
  , disabledA = [Italic] }


-- | Basic 8-bit colors
black, white, red, lime , blue, yellow, cyan, magenta, silver, gray, maroon, olive, green, purple, teal, navy :: Color
black 	= "#000000"
white 	= "#FFFFFF"
red 	= "#FF0000"
lime 	= "#00FF00"
blue 	= "#0000FF"
yellow 	= "#FFFF00"
cyan    = "#00FFFF"
magenta = "#FF00FF"
silver 	= "#C0C0C0"
gray 	= "#808080"
maroon 	= "#800000"
olive 	= "#808000"
green 	= "#008000"
purple 	= "#800080"
teal 	= "#008080"
navy 	= "#000080"


