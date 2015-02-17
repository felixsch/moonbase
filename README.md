#Moonbase — Desktop in a Haskell file

![Screenshot of the default configuration](http://none.io/share/moonbase1.png)

A desktop environment aiming to be easily configuriered. Moonbase is not a application. It is a Monad which makes it easy to define your own desktop environment with your own needs.

> Currently Moonbase is just a mock up. Do __not__ use it in production it might and will crash!

###Moonbase comes in several packages
Because not everybody needs all of the stuff moonbase support, moonbase is splitted into several packages. You don't need the Gtk packages if you not interessted in a Gtk based desktop. This packages just the start. More coming (hopefully soon!)!
 - `moonbase` — Core implementation
 - `moonbase-gtk` — Gtk Desktop and widgets
 - `moonbase-xmonad` — XMonad integration

###__Try Moonbase__

Currently Moonbase is not really usable but if you want to test it anyway you can build it in two ways:

>Before you can start, `moonbase` depends on `xdg` which is not on hackage (Get it from [http://github.com/felixsch/xdg](http://github.com/felixsch/xdg)).


Download the latest `moonbase-testing` package from here [http://github.com/felixsch/moonbase-testing](http://github.com/felixsch/moonbase-testing) and:

    mkdir moonbase && cd moonbase
    
    git clone git@github.com/felixsch/xdg
    git clone git@github.com/felixsch/moonbase
    git clone git@github.com/felixsch/moonbase-gtk
    git clone git@github.com/felixsch/moonbase-xmonad
    git clone git@github.com/felixsch/moonbase-testing
    
    cd moonbase-test
    
    cabal sandbox init
    cabal sandbox add-source ../xdg
    cabal sandbox add-source ../moonbase
    cabal sandbox add-source ../moonbase-gtk
    cabal sandbox add-source ../moonbase-xmonad
    cabal install --dependencies-only
    cabal build
    
> Configuration is found in `moonbase-testing/basic/Main.hs`


###__How a configuration can look like__

```
-- moonbase
import Moonbase
import Moonbase.Theme
import Moonbase.Preferred
import Moonbase.Service
import Moonbase.Desktop.Gtk

-- moonbase-gtk
import Moonbase.Panel.Gtk
import Moonbase.Panel.Gtk.Item.Date
import Moonbase.Panel.Gtk.Item.Spacer
import Moonbase.Panel.Gtk.Item.CpuGraph
import Moonbase.Panel.Gtk.Item.DbusLabel

import Moonbase.Prompt.Gtk

-- moonbase-xmonad
import Moonbase.WindowManager.XMonad

myConfig = Config { terminal = "urxvt" }

main :: IO ()
main = moonbase myConfig $ do
    setTheme defaultTheme
    withPreferred $ [ mimeImages                 ==> app "gimp"
                    , (mimeVideos <> mimeAudios) ==> app "vlc"
                    , mimePdf                    ==> app "evince"
                    , mime "text/plain"          ==> app "gvim"]
                    
    gtkDesktop $ onEvery (wallpaper "/path/to/favorite/wallpaper.png")
    gtkPanel_ $ xmonadLog <> spacer <> cpuGraph 250 <> date " [%d %m %Y %H:%M:%S] "
    
    gtkPrompt id
    
    withXMonad $ default_ $ \conf -> additionalKeysP conf
        [ ("M-p", Moonbase.io $ showPrompt) ]
        
    atStartup $ autostart [app "xterm", app "firefox"]
```



