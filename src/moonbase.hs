


import qualified Data.Map as M

import Moonbase
import Moonbase.Theme
import Moonbase.Service
import Moonbase.Preferred

moConfig :: Config
moConfig = Config
  { terminal = "xterm"
  }

main :: IO ()
main = moonbase moConfig $ do
    setTheme defaultTheme
    withPreferred $ [ mimeImages ==> app "gimp" ]
    atStartup $ setRootWindowColor



    
