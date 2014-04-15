


import System.IO


import Moonbase
import Moonbase.Core


data DummyWM = DummyWM String
instance WindowManagerClass DummyWM where
    startWM (DummyWM n) = io $ putStrLn $ "Start dummyWM: " ++ n
    stopWM  (DummyWM n) = io $ putStrLn $ "Stop dummyWM: " ++ n

defaultConfig :: MoonConfig 
defaultConfig
    = MoonConfig
    { windowManager = WindowManager $ DummyWM "testing"
    }

main :: IO ()
main = moonbase defaultConfig
