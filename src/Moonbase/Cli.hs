{-# LANGUAGE TemplateHaskell #-}

module Moonbase.Cli where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

import           Control.Applicative
import           Control.Lens              hiding (argument)

import           Options.Applicative

import           Data.Maybe
import           Moonbase.DBus

import qualified DBus
import qualified DBus.Client               as DBus

import           System.Exit

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

runCommand :: String -> [String] -> IO ()
runCommand cmd args = do
   client <- connectDBus moonbaseCliBusName

   when (isNothing client) $ die "Could not connect to DBus"

   reply <- DBus.call (fromJust client) (DBus.methodCall (withObjectPath "Action") (withInterface "Action") (DBus.memberName_ "RunAction"))
     { DBus.methodCallDestination = Just moonbaseBusName
     , DBus.methodCallBody        = [DBus.toVariant (cmd:args)]
     }
   case reply of
     Left err    ->  die $ DBus.methodErrorMessage err
     Right reply ->  case replyValue reply of
       Nothing -> die "Could not decode moonbase reply. This indicates a bug, report this if the error persists"
       Just x  -> putStrLn x
   where
     replyValue reply    = DBus.fromVariant (head $ DBus.methodReturnBody reply)

runCli :: (Bool -> IO ()) -> IO ()
runCli start = do
  options <- parseOptions
  if options ^. cmd  == "start"
    then start (options ^. verbose)
    else runCommand (options ^. cmd) (options ^. args)
