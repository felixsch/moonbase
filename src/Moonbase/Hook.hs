module Moonbase.Hook
    ( loadHooks
    , runHooks
    , dbusListAllHooks
    ) where

import Control.Applicative
import Control.Monad.State

import Data.List

import Moonbase.Core
import Moonbase.Log


loadHooks :: Moonbase ()
loadHooks
    = do
        h <- allHooks
        modify (\conf -> conf { hks = h })
    where
        allHooks = nub . concat <$> sequence [one desktop, more autostart, more panels, one windowManager, hooks <$> askConf]
        one t = requires . t <$> askConf
        more t = concatMap requires . t <$> askConf


runHooks :: HookType -> Moonbase ()
runHooks
    t = mapM_ enable' =<< (filter (byType t) . hks <$> get)
    where
        byType a (Hook _ b _ ) = a == b
        enable' (Hook a ty call) = do
            infoM $ "[" ++ show ty ++ "] hook '" ++ a ++ "' started"
            call


dbusListAllHooks :: Moonbase [(String,String)]
dbusListAllHooks
    = map (\(Hook n t _) -> (show t, n)) . hks <$> get
            
