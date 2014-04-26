module Moonbase.Log
    ( LogTag(..)
    , Logger(..)
    ) where


data LogTag = ErrorTag
            | InfoTag
            | WarningTag
            | CustomTag String
            deriving (Eq)

instance Show LogTag where
    show ErrorTag      = "(Err)  "
    show InfoTag       = "       "
    show WarningTag    = "(Warn) "
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
