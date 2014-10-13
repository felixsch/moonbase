


import qualified Data.Map as M

import Moonbase
import Moonbase.Service
import Moonbase.Preferred

moConfig :: Config
moConfig = Config {}

main :: IO ()
main = moonbase moConfig $ do
    withPreferred $ [ mimeImages ==> app "gimp" ]
