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


handlePanelError :: PanelError -> Moonbase ()
handlePanelError (PanelError msg) = errorM $ " PanelError: " ++ msg


startPanel :: Panel -> Moonbase ()
startPanel (Panel n st) = do
    infoM $ "Starting panel: " ++ n
    sta <- runPanelT start st
    case sta of
        Left err -> handlePanelError err
        Right (started, nst) -> if started
            then modify (\x -> x { stPanels = M.insert n (Panel n nst) (stPanels x) })
            else warnM $ "Starting panel " ++ n ++ " failed!"

stopPanel :: Panel -> Moonbase ()
stopPanel (Panel n st) = do
    debugM $ "Stoping panel " ++ n ++ "..."
    _ <- runPanelT stop st
    modify (\x -> x { stPanels = M.delete n (stPanels x)})


startPanels :: Moonbase ()
startPanels = mapM_ startPanel =<< panels <$> askConf


stopPanels :: Moonbase ()
stopPanels = mapM_ stopPanel =<< M.elems . stPanels <$> get



getPanel :: Name -> Moonbase (Maybe Panel)
getPanel
    n = M.lookup n . stPanels <$> get

putPanel :: Panel -> Moonbase () 
putPanel 
    p@(Panel n _) = modify (\st -> st { stPanels = M.insert n p (stPanels st) })

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
    n = M.member n . stPanels <$> get

dbusListRunningPanels :: Moonbase [String]
dbusListRunningPanels
    = M.keys . stPanels <$> get


dbusListAllPanels :: Moonbase [String]
dbusListAllPanels
    = map (\(Panel n _) -> n) . panels <$> askConf


dbusStartPanel :: Name -> Moonbase ()
dbusStartPanel
    n = do
        notR <- not <$> isPanelRunning n
        when notR (start' =<< askPanel n)
    where
        
        start' Nothing  = warnM $ "Could not start panel " ++ n ++ ": panel unknown"
        start' (Just p) = startPanel p



dbusStopPanel :: Name -> Moonbase ()
dbusStopPanel
    n = perform =<< getPanel n
    where
        perform Nothing = warnM $ "Could not stop panel " ++ n ++ ": panel unknown"
        perform (Just p) = do
            debugM $ "dbus --> stoping panel: " ++ n
            stopPanel p
            modify (\st -> st { stPanels = M.delete n (stPanels st)})
        
        
          
          




