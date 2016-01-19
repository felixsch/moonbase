{-# LANGUAGE TemplateHaskell #-}

module Moonbase.Util.TH
  ( which
  , exists
  ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Control.Applicative
import           Data.Bool
import           Data.Maybe
import           System.Directory

which = QuasiQuoter
  { quoteExp  = \s -> maybe (notFound s) (\path -> [|path|]) =<< runIO (findExecutable s)
  , quotePat  = invalid
  , quoteDec  = invalid
  , quoteType = invalid
  }
  where
   invalid       = error "Can only check for values nothing other"
   notFound exec = error $ "\n*** ERROR *** Could not find executable `" ++ exec ++"`. Make sure the application is installed correctly\n"

exists = QuasiQuoter
  { quoteExp  = \s -> bool (notFound s) [|s|] =<< runIO ((||) <$> doesFileExist s <*> doesDirectoryExist s)
  , quotePat  = invalid
  , quoteDec  = invalid
  , quoteType = invalid
  }
  where
   invalid       = error "Can only check for values nothing other"
   notFound path = error $ "\n*** ERROR *** File or Directory not found: " ++ path
