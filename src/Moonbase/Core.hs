module Moonbase.Core where






import Control.Monad.Reader
import Control.Monad.State


data MoonState = MoonState {
  name :: String

}


data MoonConfig = MoonConfig {

 nname :: String

}
