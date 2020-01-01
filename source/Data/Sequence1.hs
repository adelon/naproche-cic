{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}


-- Intended for qualified import:
-- import Data.Sequence1 (Seq1(..))
-- import qualified Data.Sequence1 as Seq1


module Data.Sequence1
  ( cons
  , snoc
  , uncons
  , unsnoc
  , toNonEmpty
  , fromNonEmpty
  , toSeq
  , singleton
  , replicate
  , length
  , append
  , map
  , pattern IsSeq1
  , pattern IsEmpty
  , Seq1 ((:<||), (:||>))
  ) where


import Data.Coerce (coerce)

import Data.Bool
import Data.Eq (Eq(..))
import Data.Foldable (Foldable)
import Data.Function
import Data.Functor (Functor(..))
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..), Ordering)
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq(..))
import Data.Traversable (Traversable(..))
import GHC.Err (error)
import GHC.Num (Num(..))
import Text.Show (Show(..))

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable


data Seq1 a = Seq1
  { seqHead :: a
  , seqTail :: !(Seq a)
  } deriving (Show, Traversable)

pattern (:<||) :: a -> Seq a -> Seq1 a
pattern x :<|| xs = Seq1 x xs
{-# COMPLETE (:<||) #-}

pattern (:||>) :: Seq a -> a -> Seq1 a
pattern xs :||> x <- (unsnoc->(!xs, x))
  where
    (x :<| xs) :||> y = x :<|| (xs :|> y)
    Empty      :||> y = y :<|| Empty
{-# COMPLETE (:||>) #-}

infixr 5 :<||
infixl 5 :||>

pattern IsSeq1 :: Seq1 a -> Seq a
pattern IsSeq1 n <- (fromSeq->Just n)
  where
    IsSeq1 n = toSeq n

pattern IsEmpty :: Seq a
pattern IsEmpty <- (Seq.null->True)
  where
    IsEmpty = Seq.empty

{-# COMPLETE IsSeq1, IsEmpty #-}

fromSeq :: Seq a -> Maybe (Seq1 a)
fromSeq = \case
  x :<| xs -> Just (x :<|| xs)
  Empty -> Nothing
{-# INLINE fromSeq #-}

-- | O(1).
cons :: a -> Seq1 a -> Seq1 a
x `cons` xs = x :<|| toSeq xs
{-# INLINE cons #-}

snoc :: Seq1 a -> a -> Seq1 a
xs `snoc` x = toSeq xs :||> x
{-# INLINE snoc #-}

uncons :: Seq1 a -> (a, Seq a)
uncons (x :<|| xs) = (x, xs)
{-# INLINE uncons #-}

unsnoc :: Seq1 a -> (Seq a, a)
unsnoc (x :<|| (xs :|> y)) = (x :<| xs, y)
unsnoc (x :<|| Empty) = (Empty, x)
{-# INLINE unsnoc #-}


toNonEmpty :: Seq1 a -> NonEmpty a
toNonEmpty (x :<|| xs) = x :| Foldable.toList xs
{-# INLINE toNonEmpty #-}

-- | O(n).
fromNonEmpty :: NonEmpty a -> Seq1 a
fromNonEmpty (x :| xs) = x :<|| Seq.fromList xs
{-# INLINE fromNonEmpty #-}

toSeq :: Seq1 a -> Seq a
toSeq (x :<|| xs) = x :<| xs
{-# INLINE toSeq #-}



instance Eq a => Eq (Seq1 a) where
  (==) :: (Seq1 a) -> (Seq1 a) -> Bool
  xs == ys = length xs == length ys && toNonEmpty xs == toNonEmpty ys

instance Ord a => Ord (Seq1 a) where
  compare :: (Seq1 a) -> (Seq1 a) -> Ordering
  compare = compare `on` toNonEmpty

instance Semigroup (Seq1 a) where
  (<>) :: Seq1 a -> Seq1 a -> Seq1 a
  (<>) = append

instance Functor Seq1 where
  fmap = map
  {-# INLINE fmap #-}
  x <$ xs = replicate (length xs) x
  {-# INLINE (<$) #-}

instance Foldable Seq1 where
  fold :: Monoid m => Seq1 m -> m
  fold (x :<|| xs) = x <> Foldable.fold xs
  {-# INLINE fold #-}

  foldMap :: Monoid m => (a -> m) -> Seq1 a -> m
  foldMap f (x :<|| xs) = f x <> Foldable.foldMap f xs
  {-# INLINE foldMap #-}

  foldr f z (x :<|| xs) = x `f` Foldable.foldr f z xs
  {-# INLINE foldr #-}

  foldr' f z (xs :||> x) = Foldable.foldr' f y xs where !y = f x z
  {-# INLINE foldr' #-}

  foldl f z (xs :||> x) = Foldable.foldl f z xs `f` x
  {-# INLINE foldl #-}

  foldl' f z (x :<|| xs) = Foldable.foldl' f y xs where !y = f z x
  {-# INLINE foldl' #-}

  foldr1 f (xs :||> x) = Foldable.foldr f x xs
  {-# INLINE foldr1 #-}

  foldl1 f (x :<|| xs) = Foldable.foldl f x xs
  {-# INLINE foldl1 #-}

  null _ = False
  {-# INLINE null #-}

  length = length
  {-# INLINE length #-}


-- | O(1). A singleton sequence.
singleton :: a -> Seq1 a
singleton = (:<|| Seq.empty)
{-# INLINE singleton #-}

replicate :: Int -> a -> Seq1 a
replicate n x
    | n < 1     = error "Seq1.replicate: must take a positive integer argument"
    | otherwise = x :<|| Seq.replicate (n - 1) x
{-# INLINE replicate #-}

-- | O(1). The number of elements in the sequence.
length :: Seq1 a -> Int
length (_ :<|| xs) = 1 + Seq.length xs
{-# INLINE length #-}


append :: Seq1 a -> Seq1 a -> Seq1 a
(x :<|| xs) `append` ys = x :<|| (xs Seq.>< toSeq ys)
{-# INLINE append #-}

map :: (a -> b) -> Seq1 a -> Seq1 b
map f (x :<|| xs) = f x :<|| fmap f xs
{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
 #-}
{-# RULES
"map/coerce" map coerce = coerce
 #-}
