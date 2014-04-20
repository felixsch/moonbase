


import System.IO
import qualified Data.Map as M

import Moonbase
import Moonbase.Core
import Moonbase.WindowManager.Generic
import Moonbase.Service.Generic


openbox :: [String] -> WindowManager
openbox args = WindowManager $ GenericWM "openbox" args Nothing


xfcePanel :: Service
xfcePanel = Service "xfce4panel" $ GenericService "xfce4-panel" [] Nothing

samplePreferred :: M.Map String Preferred
samplePreferred
    = M.fromList
        [ ("image/png", AppName "ristretto") ]


defaultConfig :: MoonConfig 
defaultConfig
    = MoonConfig
    { windowManager = openbox []
    , autostart = [xfcePanel]
    , preferred = samplePreferred
    }

main :: IO ()
main = moonbase defaultConfig
