module Moonbase.Log
    ( LogTag(..)
    , Logger(..)
    , formatMessage
    ) where

import System.IO

import Data.Time.Format
import Data.Time.LocalTime

import Control.Applicative 
import Control.Monad.Reader

data LogTag = ErrorTag
            | InfoTag
            | WarningTag
            | DebugTag
            | CustomTag String
            deriving (Eq)

instance Show LogTag where
    show ErrorTag      = "(Err)  "
    show InfoTag       = "       "
    show WarningTag    = "(Warn) "
    show DebugTag      = "(Debug)"
    show (CustomTag msg) = "(" ++ msg ++ ") " 


class (Monad m, MonadIO m) => Logger m where

    getVerbose :: m Bool
    getLogName :: m (Maybe String)
    getHdl :: m Handle
 
    logM :: LogTag -> String -> m ()
    logM tag msg = do
        verbose <- getVerbose
        name <- getLogName
        hdl <- getHdl
        
        message <- formatMessage tag name msg

        liftIO $ hPutStrLn hdl message >> hFlush hdl
        when verbose $ liftIO $ putStrLn message

    debugM :: String -> m ()
    debugM msg = do
        hdl <- getHdl
        verbose <- getVerbose
        name <- getLogName

        message <- formatMessage DebugTag name msg

        when verbose $ liftIO $ hPutStrLn hdl message >> hFlush hdl

    errorM :: String -> m ()
    errorM = logM ErrorTag

    infoM :: String -> m ()
    infoM = logM InfoTag

    warnM :: String -> m ()
    warnM = logM WarningTag




formatMessage :: (MonadIO m) => LogTag -> Maybe String -> String -> m String
formatMessage tag mo message = liftIO $ do
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> getZonedTime
    return $ case mo of
        Just m  -> "[" ++ date ++ "][" ++ m ++ "] " ++ show tag ++ ": " ++ message
        Nothing -> "[" ++ date ++ "] " ++ show tag ++ ": " ++ message
