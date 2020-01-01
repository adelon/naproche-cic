module Language.PatternTree where


import qualified Data.Sequence1 as Seq1
import qualified Data.Set1 as Set1
import qualified Data.Text as Text

data Shape
  = Slot
  | Word [Text]
  deriving (Show, Eq, Ord)

instance IsString Shape where
  fromString w = Word [Text.pack w]

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

singleton :: Pattern -> Patterns
singleton pat = Set1.singleton case pat of
  shape :<|| IsEmpty -> PatternContinue shape $ Set1.singleton PatternEnd
  shape :<|| (IsSeq1 pat') -> PatternContinue shape $ singleton pat'

makePattern :: NonEmpty Shape -> Pattern
makePattern = Seq1.fromNonEmpty

insertPattern :: Pattern -> Patterns -> Patterns
insertPattern = error "insertPattern undefined"
