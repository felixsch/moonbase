


import System.IO


import Moonbase
import Moonbase.Core
import Moonbase.WindowManager.Generic
import Moonbase.Service.Generic


openbox :: [String] -> WindowManager
openbox args = WindowManager $ GenericWM "openbox" args Nothing


xfcePanel :: Service
xfcePanel = Service "xfce4panel" $ GenericService "xfce4-panel" [] Nothing



defaultConfig :: MoonConfig 
defaultConfig
    = MoonConfig
    { windowManager = openbox []
    , autostart = [xfcePanel]
    }

main :: IO ()
main = moonbase defaultConfig
