{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
module Data.SetLike where

import           Data.Hashable
import           Data.HashSet (HashSet(..))
import qualified Data.HashSet as HS
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Monoid

class Monoid full => SetLike full item | full -> item where
    member      :: item -> full -> Bool
    singleton   :: item -> full
    (\\)        :: full -> full -> full

instance (Eq item, Hashable item) => SetLike (HashSet item) item where
    member      = HS.member
    singleton   = HS.singleton
    (\\)        = HS.difference

instance (Ord item) => SetLike (Set item) item where
    member      = S.member
    singleton   = S.singleton
    (\\)        = (S.\\)

instance SetLike IntSet Int where
    member      = IS.member
    singleton   = IS.singleton
    (\\)        = (IS.\\)
