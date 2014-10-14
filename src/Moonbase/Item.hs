module Moonbase.Item
  ( Item(..)
  , Additional(..)
  , item
  , additional
  , (<>)
  ) where

import Data.Monoid

-- | A generic Item definition which is used to implement item structures such as
-- panel items or prompt definitions
-- 
-- It stores a list of (monadic) actions and preserves order
data Item m a = Item [m a]

instance Monoid (Item m a) where
    mempty = Item []
    mappend (Item i1) (Item i2) = Item (i1 ++ i2)

-- | A gerneric Additional item which is used to implement some additions to a structure such as additional widgets in a prompt
--
-- It stores a list of (monadic) actions and preserves order
data Additional m a = Additional [m a]

instance Monoid (Additional m a) where
    mempty = Additional []
    mappend (Additional i1) (Additional i2) = Additional (i1 ++ i2)

-- | Generates a 'Item' out of a function
item :: m a -> Item m a 
item f = Item [f]

-- | Generates a 'Addtitional' out of a function
additional :: m a -> Additional m a
additional f = Additional [f]


