{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
module Data.MapLike where

import           Prelude hiding (lookup)
import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Lazy as HL
--import qualified Data.HashMap.Strict as HS
import qualified Data.Map as M
import qualified Data.IntMap as IM
import           Data.Monoid hiding ((<>))
import           Data.Semigroup

data Alter a = Keep | Remove | Replace a
  deriving (Read, Show, Eq, Ord)

-- | Converts 'Nothing' to 'Remove' and 'Just' to 'Replace'.
maybeToAlter :: Maybe a -> Alter a
maybeToAlter = maybe Remove Replace
{-# INLINE maybeToAlter #-}

-- | Minimal complete definition: ('empty', 'unionWith' and 'alterF') or
-- ('empty', 'unionWith', 'lookup', 'insert' and 'delete').
class MapLike full key item | full -> key item where
    empty       :: full
    member      :: key -> full -> Bool
    member      = (maybe False (const True) .) . lookup
    lookup      :: key -> full -> Maybe item
    lookup k    = getConstant . alterF k Constant
    insert      :: key -> item -> full -> full
    insert k v  = runIdentity . alterF k (const $ Identity (Replace v))
    delete      :: key -> full -> full
    delete k    = runIdentity . alterF k (const $ Identity Remove)
    singleton   :: key -> item -> full
    singleton k v = insert k v empty
    --(\\)        :: full -> full -> full
    -- | Note also that @alterF :: key -> Lens full full (Maybe item) (Alter item)@
    -- (see the @lens@ package).
    alterF      :: (Functor f) => key -> (Maybe item -> f (Alter item)) -> (full -> f full)
    alterF k f m = fmap ins (f v)
      where
        v               = lookup k m
        ins Keep        = m
        ins Remove      = delete k m
        ins (Replace x) = insert k x m
    {-# INLINE alterF #-}
    unionWith   :: (item -> item -> item) -> (full -> full -> full)

{-
instance (Eq key, Hashable key) => MapLike (HS.HashMap key item) key item where
    lookup      = HS.lookup
    singleton   = HS.singleton
    --(\\)        = HS.difference
-}

instance (Eq key, Hashable key) => MapLike (HL.HashMap key item) key item where
    empty       = HL.empty
    lookup      = HL.lookup
    delete      = HL.delete
    insert      = HL.insert
    singleton   = HL.singleton
    unionWith   = HL.unionWith
    --(\\)        = HL.difference

instance (Ord key) => MapLike (M.Map key item) key item where
    empty       = M.empty
    lookup      = M.lookup
    delete      = M.delete
    insert      = M.insert
    singleton   = M.singleton
    unionWith   = M.unionWith
    --(\\)        = (S.\\)

instance MapLike (IM.IntMap item) Int item where
    empty       = IM.empty
    lookup      = IM.lookup
    delete      = IM.delete
    insert      = IM.insert
    singleton   = IM.singleton
    unionWith   = IM.unionWith
    --(\\)        = (S.\\)

-- | Represents maps that are unioned using a semigroup operation defined on
-- their keys.  The reason for this wrapper is that monoidal operations on maps
-- are usually defined as biased replacing of values, instead of combining
-- them.
newtype MonoidMap m key item = MonoidMap { getMonoidMap :: m }
instance (Semigroup item, MapLike full key item) => Semigroup (MonoidMap full key item) where
    (MonoidMap x) <> (MonoidMap y) = MonoidMap (unionWith (<>) x y)
instance (Semigroup item, MapLike full key item) => Monoid (MonoidMap full key item) where
    mempty  = MonoidMap empty
    mappend = (<>)
