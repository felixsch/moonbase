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
        modify (\conf -> conf { stHooks = h })
    where
        allHooks = nub . concat <$> sequence [one desk, more autostart, more panels, one wm, hooks <$> askConf]
        one t = requires . t <$> askConf
        more t = concatMap requires . t <$> askConf


runHooks :: HookType -> Moonbase ()
runHooks
    t = mapM_ enable' =<< (filter (byType t) . stHooks <$> get)
    where
        byType a (Hook _ b _ ) = a == b
        enable' (Hook a ty call) = do
            infoM $ "[" ++ show ty ++ "] hook '" ++ a ++ "' started"
            call


dbusListAllHooks :: Moonbase [(String,String)]
dbusListAllHooks
    = map (\(Hook n t _) -> (show t, n)) . stHooks <$> get
            
