module Language.Pattern where


import Data.Sequence1 as Seq1
import Data.String (IsString(..))
import Data.Text (Text, pack)


data Shape
  = Slot
  | Word [Text]
  deriving (Show, Eq, Ord)

instance IsString Shape where
  fromString w = Word [pack w]

type Patterns = Set1 PatternTree

data PatternTree
  = PatternEnd
  | PatternContinue Shape Patterns
  deriving (Eq, Ord, Show)

type Pattern = Seq1 Shape

-- Note that the Ord instance is defined so that for overlapping
-- patterns, the one with more words is greater than the one with
-- an earlier variable slot.
--
-- This means we can try parsers derived from patterns in decreasing order
-- and be sure that we get the longest match possible.

insertPattern :: Pattern -> Patterns -> Patterns
insertPattern pat pats = error "Pattern.insert incomplete"