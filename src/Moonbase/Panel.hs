module Moonbase.Panel
    ( startPanels, stopPanels
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



startPanel :: Panel -> Moonbase ()
startPanel (Panel n _ st) = do
    debugM $ "Starting panel: " ++ n

    ref <- newRef st
    status <- runComponentM n ref start
    if status
        then modify (\x -> x { stPanels = M.insert n (RefWrapper ref) (stPanels x)})
        else warnM $ "Starting panel: " ++ n ++ " failed!"

stopPanel :: Name -> Moonbase ()
stopPanel n = do
    runtime <- get
    debugM $ "Stoping panel " ++ n ++ "..."

    case (M.lookup n (stPanels runtime)) of
        Just (RefWrapper ref) -> do
            _ <- runComponentM n ref stop
            modify (\x -> x { stPanels = M.delete n (stPanels x)})
        Nothing  -> debugM $ "Trying to stop not existing panel: " ++ n


startPanels :: Moonbase ()
startPanels = mapM_ startPanel =<< panels <$> askConf


stopPanels :: Moonbase ()
stopPanels = mapM_ stopPanel =<< M.keys . stPanels <$> get


askPanel :: Name -> Moonbase (Maybe Panel)
askPanel
    n = search . panels <$> askConf
    where
        search [] = Nothing
        search (p@(Panel ns _ _): xs)
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
    = map (\(Panel n _ _) -> n) . panels <$> askConf


dbusStartPanel :: Name -> Moonbase ()
dbusStartPanel
    n = do
        notR <- not <$> isPanelRunning n
        when notR (start' =<< askPanel n)
    where
        
        start' Nothing  = warnM $ "Could not start panel " ++ n ++ ": panel unknown"
        start' (Just p) = startPanel p

dbusStopPanel :: Name -> Moonbase ()
dbusStopPanel = stopPanel
        
        
          
          




