


import qualified Data.Map as M

import Moonbase
import Moonbase.Preferred (app)
import Moonbase.WindowManager
import Moonbase.Service
import Moonbase.Desktop


moPreferred :: M.Map String Preferred
moPreferred
    = M.fromList
        [ ("image/png", app "gimp") ]

moConfig :: Config
moConfig = Config {}

main :: IO ()
main = moonbase moConfig $ do
    return ()
