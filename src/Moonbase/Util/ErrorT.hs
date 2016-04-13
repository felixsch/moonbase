{-# LANGUAGE MultiParamTypeClasses #-}

module Moonbase.Util.ErrorT
  (
  ) where

import           Control.Monad.Trans.Either
import           Moonbase.Core
import           System.Exit


type ErrorT  = EitherT

indicate :: (Monad m) => e -> ErrorT e m a
indicate = left

success :: (Monad m) => a -> ErrorT e m a
success = return

try :: ErrorT e m a -> m (Either e a)
try = runEitherT

eitherToError :: (Monad m) => Either e a -> ErrorT e m a
eitherToError = either indicate success

maybeToError :: (Monad m) => e -> Maybe a -> ErrorT e m a
maybeToError err = maybe (indicate err) success

whenFail :: Monad m => ErrorT e m a -> (e -> m a) -> m a
whenFail f fallback = do
  result <- try f
  case result of
    Left err    -> fallback err
    Right value -> return value


type CanFail rt m a = ErrorT (String, Maybe ExitCode) (MB rt m) a

fail :: (Moonbase rt m) => String -> CanFail rt m a
fail msg = indicate (msg, Nothing)

fatal :: (Moonbase rt m) => String -> CanFail rt m a
fatal msg = indicate (msg, Just (ExitFailure 1))

