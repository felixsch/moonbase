{-|
Module      : Moonbase.Service
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX

If you want to start or run an action at some point. This is called 'Service'

They can used to setup a basic working environment

for Example:

> main :: IO ()
> main = moonbase myConfig $ do
>     setTheme myTheme
>     withPreferred [ mimeImages              ==> app "gimp"
>                   , mimeVideos <> mimeAudio ==> app "vlc"
>                   , mimeSources <> mimeTxt  ==> terminal "vim" ]
>     atStartup $ pulseaudio
>               <> setLanguage "us" "altgr-intl"
>               <> setRootWindowColor "#000000"
>               <> autostart [terminal', app "firefox", terminal "vim"]
-}

module Moonbase.Service
    ( pulseaudio
    , setLanguage
    , setRootWindowColor
    ) where

import Control.Monad
import Data.Maybe


import Moonbase
import Moonbase.Item
import Moonbase.Util.Application


-- | A Service which could be executed at startup or when triggering a certain key
type Service = Item Moonbase ()

-- | Run a 'Service' in 'Moonbase'
run :: Service -> Moonbase ()
run (Item services) = sequence_ services

-- | Configure services which should run at startup
atStartup :: Service -> Moonbase ()
atStartup (Item services) = withComponent "startup" $ newComponent services $ 
    mapM_ moon services

-- | Start pulseaudio if not started
pulseaudio :: Service
pulseaudio = item $ void $ spawn $ app "start-pulseaudio-x11"

-- | Set X11 language
setLanguage :: String -> Maybe String -> Service
setLanguage lang variant = item $ void $ spawn $ appWith "setxkbmap" (lang : arguments variant)
  where
      arguments (Just v) = ["-variant", v]
      arguments Nothing  = []

-- | Set root window color
setRootWindowColor :: Service
setRootWindowColor = item $ do
    theme <- getTheme
    void $ spawn $ appWith "xsetroot" ["-solid", bg theme]

-- | Autostart some applications
autostart :: [Application] -> Service
autostart apps = item $ mapM_ spawn apps
