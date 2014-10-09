


import qualified Data.Map as M

import Moonbase
import Moonbase.Preferred (app)
import Moonbase.WindowManager.Generic
import Moonbase.Service.Generic
import Moonbase.Desktop.Generic


openbox :: [String] -> WindowManager
openbox = newGenericWM "openbox"

xfcePanel :: Service
xfcePanel = newGenericService "xfce4-panel" []

samplePreferred :: M.Map String Preferred
samplePreferred
    = M.fromList
        [ ("image/png", app "gimp") ]

setRoot :: String -> Desktop
setRoot 
    color = newGenericDesktop "xsetroot" ["-solid", color]


defaultConfig :: Config 
defaultConfig
    = Config
    { wm = openbox []
    , autostart = [xfcePanel]
    , preferred = samplePreferred
    , desk   = setRoot "#ff0000"
    , panels = []
    , hooks = []
    }

main :: IO ()
main = moonbase defaultConfig
