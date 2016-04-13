module Main where

import           Moonbase

main :: IO ()
main = moonbase $ do
  puts "Test Moonbase"
  delay 200000
  puts "Haha"
  quit (ExitFailure 12)
