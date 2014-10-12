


import qualified Data.Map as M

import Moonbase
import Moonbase.Service
import Moonbase.Preferred (app)


moPreferred :: M.Map String Preferred
moPreferred
    = M.fromList
        [ app "image/png" "gimp" ]

moConfig :: Config
moConfig = Config {}

main :: IO ()
main = moonbase moConfig $ do
    return ()
