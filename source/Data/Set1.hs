{-# LANGUAGE NoImplicitPrelude #-}

-- Intended for qualified import:
-- import Data.Set1 (Set1)
-- import qualified Data.Set1 as Set1

module Data.Set1
  ( Set1
  , singleton
  , insertSmallest
  , toNonEmpty
  , toSet
  , fromNonEmpty
  , fromAscNonEmpty
  , toDescNonEmpty
  , elim
  ) where


import BasePrelude hiding (union)
import Data.Set (Set)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set


data Set1 a = Set1
  { set1Smallest :: !a
  , set1Rest :: !(Set a)
  } deriving (Show, Typeable)

instance Eq a => Eq (Set1 a) where
  xs1 == xs2 = Set.size (set1Rest xs1) == Set.size (set1Rest xs2)
    && toNonEmpty xs1 == toNonEmpty xs2

instance Ord a => Ord (Set1 a) where
  compare = compare `on` toNonEmpty
  (<) = (<) `on` toNonEmpty
  (>) = (>) `on` toNonEmpty
  (<=) = (<=) `on` toNonEmpty
  (>=) = (>=) `on` toNonEmpty

-- | Left-biased union.
instance Ord a => Semigroup (Set1 a) where
  (<>) = union
  {-# INLINE (<>) #-}

singleton :: Ord a => a -> Set1 a
singleton x = Set1 x mempty

insertSmallest :: a -> Set a -> Set1 a
insertSmallest = Set1

toNonEmpty :: Set1 a -> NonEmpty a
toNonEmpty (Set1 x xs) = x :| Set.toList xs
{-# INLINE toNonEmpty #-}

toSet :: Ord a => Set1 a -> Set a
toSet (Set1 x xs) = Set.insert x xs
{-# INLINE toSet #-}

-- | /O(n)/. Convert the set to a descending non-empty list of elements.
toDescNonEmpty :: Set1 a -> NonEmpty a
toDescNonEmpty (Set1 x xs) = Set.foldl' (flip NonEmpty.cons) (x :| []) xs
{-# INLINE toDescNonEmpty #-}

fromSet :: Set a -> Maybe (Set1 a)
fromSet xs = case Set.minView xs of
  Nothing -> Nothing
  Just (x, xs') -> Just (insertSmallest x xs')

fromNonEmpty :: Ord a => NonEmpty a -> Set1 a
fromNonEmpty (x :| xs) = elim (singleton x) (<> singleton x) . Set.fromList $ xs
{-# INLINE fromNonEmpty #-}

elim :: b -> (Set1 a -> b) -> Set a -> b
elim defaultElem f = maybe defaultElem f . fromSet

fromAscNonEmpty :: Eq a => NonEmpty a -> Set1 a
fromAscNonEmpty (x :| xs) = Set1 x (Set.fromAscList xs)
{-# INLINE fromAscNonEmpty #-}


union :: Ord a => Set1 a -> Set1 a -> Set1 a
union n1@(Set1 x1 s1) n2@(Set1 x2 s2) = case compare x1 x2 of
    LT -> Set1 x1 . Set.union s1 . toSet $ n2
    EQ -> Set1 x1 . Set.union s1         $ s2
    GT -> Set1 x2 . Set.union (toSet n1) $ s2
{-# INLINE union #-}
