


import System.IO


import Moonbase
import Moonbase.Core


data DummyWM = DummyWM String
instance WindowManagerClass DummyWM where
    startWM _ = io $ putStrLn "Start dummyWM"
    stopWM  _ = io $ putStrLn "Stop dummyWM"

defaultConfig :: MoonConfig 
defaultConfig
    = MoonConfig
    { windowManager = WindowManager $ DummyWM "testing"
    }

main :: IO ()
main = moonbase defaultConfig
