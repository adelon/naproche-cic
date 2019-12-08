module Language.Pattern (Pattern(..), Shape(..)) where


import Data.Text (Text, pack)
import Data.String (IsString(..))

data Shape
  = Slot
  | Word [Text] -- List of acceptable synonyms
  deriving (Show, Eq, Ord)

instance IsString Shape where
  fromString w = Word [pack w]

newtype Pattern = Pattern { unPattern :: [Shape]}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

-- Note that the Ord instance is defined so that for overlapping
-- patterns, the one with more words is greater than the one with
-- an earlier variable slot. For example:
--
-- Pattern [Slot, "converges"] `compare` Pattern [Slot, "converges", "to", Slot]
-- --> LT
--
-- This means we can try parsers derived from patterns in decreasing order
-- and be sure that we get the longest match possible.