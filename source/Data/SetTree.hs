module Data.SetTree where


import Prelude hiding (Tree(..), map)

import qualified Data.Set as Set


data SetTree a = Node
  { label :: a
  , subForest :: SetForest a
  } deriving (Eq, Show)

type SetForest a = Set (SetTree a)


instance Eq1 SetTree where
  liftEq eq = go
    where
      go (Node a fr) (Node a' fr') = eq a a' && liftEq go fr fr'

instance Ord1 SetTree where
  liftCompare cmp = go
    where
      go (Node a fr) (Node a' fr') = cmp a a' <> liftCompare go fr fr'


map :: Ord (SetTree b) => (a -> b) -> SetTree a -> SetTree b
map f (Node a as) = Node (f a) (Set.map (map f) as)