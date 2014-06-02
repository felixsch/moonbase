module Moonbase.Log
    ( LogTag(..)
    , Logger(..)
    , formatMessage
    ) where

import System.Locale (defaultTimeLocale, rfc822DateFormat)

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)

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


class (Monad m) => Logger m where
    logM :: LogTag -> String -> m ()
    debugM :: String -> m ()

    errorM :: String -> m ()
    errorM = logM ErrorTag

    infoM :: String -> m ()
    infoM = logM InfoTag

    warnM :: String -> m ()
    warnM = logM WarningTag



formatMessage :: (MonadIO m) => LogTag -> Maybe String -> String -> m String
formatMessage tag mo message = liftIO $ do
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
    return $ case mo of
        Just m  -> "[" ++ date ++ "][" ++ m ++ "] " ++ show tag ++ ": " ++ message
        Nothing -> "[" ++ date ++ "] " ++ show tag ++ ": " ++ message
