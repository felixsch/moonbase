{-# LANGUAGE MultiParamTypeClasses #-}

module Moonbase.Util.ErrorT
  ( CanFail(..)
  , done, fail
  , eitherToFail, maybeToFail
  , whenFailed, maybeFailed
  , canFail
  ) where

import           Control.Monad.Trans.Either
import           Moonbase.Core
import           Moonbase.Util
import           Prelude                    hiding (fail, log)
import           System.Exit

type CanFail e rt m a = EitherT e (MB rt m) a

done :: (Moonbase rt m) => a -> CanFail e rt m a
done = return

fail :: (Moonbase rt m, Show e) => e -> CanFail e rt m a
fail err = lift (log (Warning $ show err)) >> left err

eitherToFail :: (Moonbase rt m, Show e) => Either e a -> CanFail e rt m a
eitherToFail = either fail done

maybeToFail :: (Moonbase rt m, Show e) => e -> Maybe a -> CanFail e rt m a
maybeToFail err = maybe (fail err) done

whenFailed :: (Moonbase rt m) => CanFail e rt m a -> (e -> MB rt m a) -> MB rt m a
whenFailed f fallback = do
  result <- runEitherT f
  case result of
    Left e  -> fallback e
    Right a -> return a

maybeFailed :: (Moonbase rt m) => CanFail e rt m a -> MB rt m (Maybe a)
maybeFailed f = do
  result <- runEitherT f
  return $ case result of
    Left _  -> Nothing
    Right a -> Just a

canFail :: (Moonbase rt m) => a -> CanFail e rt m a -> MB rt m a
canFail def f = whenFailed f (\_ -> return def)
