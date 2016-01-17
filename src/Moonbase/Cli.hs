{-# LANGUAGE TemplateHaskell #-}

module Moonbase.Cli where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Control.Lens hiding (argument)
import Control.Applicative

import Options.Applicative

import Data.Maybe
import Moonbase.DBus

import qualified DBus
import qualified DBus.Client as DBus

import System.Exit

data Options = Options
  { _verbose :: Bool
  , _cmd     :: String
  , _args    :: [String] }

makeLenses ''Options

parseOptions :: IO Options
parseOptions = execParser full
  where
    full    = info (helper <*> opts) ( fullDesc <>
                                          progDesc "moonbase managament tool" <>
                                          header "moonbase - A desktop environment" )
    opts = Options
      <$> switch (  long "verbose"
                 <> help "Print verbose messages to stdout" )
      <*> strArgument (  metavar "ACTION"
                      <> help actionHelp'
                      <> value "start" )
      <*> many (argument str (  metavar "ARGS..."
                             <> help argsHelp))

    argsHelp    ="Arguments for a action. Show action help by help ACTION"
    actionHelp' = "Run specified action. Show all commands available with list-actions"

{-
runMoonbaseAction :: Name -> [String] -> IO (Maybe String)
runMoonbaseAction name args' = do
    client <- connectSession

    reply <- call_ client (methodCall (withObjectPath "Action") (withInterface "Action") (memberName_ "RunAction"))
      { methodCallDestination = Just moonbaseBusName
      , methodCallBody        = [toVariant (name:args')] }

    let (Just reply') = fromVariant (methodReturnBody reply !! 0)

    return $ if (not $ null reply')
                then Just reply'
                else Nothing
-}
runCommand :: String -> [String] -> IO (Maybe String)
runCommand cmd args = runMaybeT $ do
   client <- liftIO $ connectDBus moonbaseCliBusName
   return "foo"

{-
runCli :: (Bool -> IO ()) -> IO ()
runCli start = do
  options <- parseOptions
  if options ^. cmd  == "start"
    then start (options ^. verbose)
    else do
      result <- runCommand (options ^. cmd) (options ^. args)
      case result of
        Just x  -> putStrLn x
        Nothing -> exitFailure "Could not execute action. DBus connection failed."
-}
