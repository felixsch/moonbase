


import System.IO


import Moonbase
import Moonbase.Core
import Moonbase.WindowManager.Generic


openbox :: [String] -> WindowManager
openbox args = WindowManager $ GenericWM "openbox" args Nothing



defaultConfig :: MoonConfig 
defaultConfig
    = MoonConfig
    { windowManager = openbox []
    }

main :: IO ()
main = moonbase defaultConfig
