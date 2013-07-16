{-# LANGUAGE FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Data.SetLike where

import           Data.Hashable
import           Data.HashSet (HashSet(..))
import qualified Data.HashSet as HS
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Monoid
import qualified Data.MapLike as ML

class Monoid full => SetLike full item | full -> item where
    member      :: item -> full -> Bool
    singleton   :: item -> full
    singleton    = flip insert mempty
    (\\)        :: full -> full -> full

    insert      :: item -> full -> full
    insert      = mappend . singleton
    delete      :: item -> full -> full
    delete      = flip (\\) . singleton

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
