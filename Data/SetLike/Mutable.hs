{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
module Data.SetLike.Mutable where

import           Prelude hiding (lookup)
import           Control.Monad
import           Control.Monad.ST
import           Data.Functor.Constant
import           Data.Functor.Identity
import qualified Data.Foldable as T
import qualified Data.Traversable as T

import           Data.Monoid hiding ((<>))
import           Data.Semigroup

class Monad m => MutableSetLike full m item | full -> m item where
    empty       :: m full
    member      :: item -> full -> m Bool
    insert      :: item -> full -> m ()
    delete      :: item -> full -> m ()
    singleton   :: item -> m full
    singleton v = empty >>= \m -> insert v m >> return m
    {-# INLINE singleton #-}

