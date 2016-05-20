module Main where

import           Control.Monad
import           Moonbase
import           Moonbase.WindowManager.XMonad
import           XMonad                        hiding (io, setLayout, spawn)

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall 1 (3/100) (3/5)

main :: IO ()
main = moonbase $ do

  withTerminal $ \args -> do
        io $ putStrLn "launching xterm"
        void $ spawn "xterm" args

  xmonad <- withXMonad ["a", "b", "c"] $ do
    setWorkspaces (map show [1..5])
    setLayout myLayout

  terminal_


