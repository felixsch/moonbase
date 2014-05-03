module Moonbase.Panel
    ( startPanels, stopPanels
    , getPanel, putPanel
    , askPanel
    , dbusListRunningPanels
    , dbusListAllPanels
    , dbusStartPanel
    , dbusStopPanel
    ) where

import Control.Applicative
import Control.Monad.State

import qualified Data.Map as M

import Moonbase.Core
import Moonbase.Log



startPanels :: Moonbase ()
startPanels
    = mapM_ start' =<< panels <$> askConf
    where
        start' (Panel n p) = do
            st <- get
            infoM $ " --: starting panel: " ++ n
            np <- start p
            modify (\x -> x { pnls = M.insert n (Panel n np) (pnls st) })


stopPanels :: Moonbase ()
stopPanels
    = mapM_ stop' =<< M.elems . pnls <$> get
    where
        stop' (Panel n p) = do
            debugM $ " --: Stoping panel: " ++ n
            stop p
            modify (\st -> st { pnls = M.delete n (pnls st)}) 


getPanel :: Name -> Moonbase (Maybe Panel)
getPanel
    n = M.lookup n . pnls <$> get

putPanel :: Panel -> Moonbase () 
putPanel 
    p@(Panel n _) = modify (\st -> st { pnls = M.insert n p (pnls st) })

askPanel :: Name -> Moonbase (Maybe Panel)
askPanel
    n = search . panels <$> askConf
    where
        search [] = Nothing
        search (p@(Panel ns _): xs)
            | ns == n   = Just p
            | otherwise = search xs

isPanelRunning :: Name -> Moonbase Bool
isPanelRunning
    n = M.member n . pnls <$> get

dbusListRunningPanels :: Moonbase [String]
dbusListRunningPanels
    = M.keys . pnls <$> get


dbusListAllPanels :: Moonbase [String]
dbusListAllPanels
    = map (\(Panel n _) -> n) . panels <$> askConf


dbusStartPanel :: Name -> Moonbase ()
dbusStartPanel
    n = do
        notR <- not <$> isPanelRunning n
        when notR (start' =<< askPanel n)
    where
        start' Nothing            = warnM $ "Could not start panel " ++ n ++ ": panel unknown"
        start' (Just (Panel _ p)) = do
            debugM $ "dbus --> starting panel: " ++ n
            np <- start p
            putPanel $ Panel n np

dbusStopPanel :: Name -> Moonbase ()
dbusStopPanel
    n = perform =<< getPanel n
    where
        perform Nothing = warnM $ "Could not stop panel " ++ n ++ ": panel unknown"
        perform (Just (Panel _ p)) = do
            debugM $ "dbus --> stoping panel: " ++ n
            stop p
            modify (\st -> st { pnls = M.delete n (pnls st)})
        
        
          
          




