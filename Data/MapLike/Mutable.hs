{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
module Data.MapLike.Mutable where

import           Prelude hiding (lookup)
import           Control.Monad
import           Control.Monad.ST
import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashTable.ST.Basic as HTB
import qualified Data.HashTable.ST.Cuckoo as HTC
import qualified Data.HashTable.ST.Linear as HTL
import qualified Data.Foldable as T
import qualified Data.Traversable as T

import           Data.Monoid hiding ((<>))
import           Data.Semigroup

import           Data.MapLike (Alter(..), maybeToAlter)

-- | Minimal complete definition: ('empty' and 'alterF') or
-- ('empty', 'lookup', 'insert' and 'delete').
class Monad m => MutableMapLike full m key item | full -> m key item where
    empty       :: m full
    member      :: key -> full -> m Bool
    member k    = liftM (maybe False (const True)) . lookup k
    {-# INLINE member #-}
    lookup      :: key -> full -> m (Maybe item)
    lookup k    = liftM getConstant . alterF k Constant
    {-# INLINE lookup #-}
    insert      :: key -> item -> full -> m ()
    insert k v  = liftM runIdentity . alterF k (const $ Identity (Replace v))
    {-# INLINE insert #-}
    delete      :: key -> full -> m ()
    delete k    = liftM runIdentity . alterF k (const $ Identity Remove)
    {-# INLINE delete #-}
    singleton   :: key -> item -> m full
    singleton k v = empty >>= \m -> insert k v m >> return m
    {-# INLINE singleton #-}
    -- | An universal modification function that unifies 'insert', 'delete' and 'lookup'.
    --
    alterF      :: (T.Traversable f) => key -> (Maybe item -> f (Alter item)) -> (full -> m (f ()))
    alterF k f m = lookup k m >>= T.mapM ins . f
      where
        ins Keep        = return ()
        ins Remove      = delete k m
        ins (Replace x) = insert k x m
    {-# INLINE alterF #-}


instance (Eq key, Hashable key) => MutableMapLike (HTB.HashTable s key item) (ST s) key item where
    empty       = HTB.new
    lookup      = flip HTB.lookup
    delete      = flip HTB.delete
    insert k v m = HTB.insert m k v

instance (Eq key, Hashable key) => MutableMapLike (HTC.HashTable s key item) (ST s) key item where
    empty       = HTC.new
    lookup      = flip HTC.lookup
    delete      = flip HTC.delete
    insert k v m = HTC.insert m k v

instance (Eq key, Hashable key) => MutableMapLike (HTL.HashTable s key item) (ST s) key item where
    empty       = HTL.new
    lookup      = flip HTL.lookup
    delete      = flip HTL.delete
    insert k v m = HTL.insert m k v
