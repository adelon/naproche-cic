{-# LANGUAGE OverloadedLists #-}

module Language.Pattern where

import qualified Data.Sequence1 as Seq1
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text


data Shape
  = Slot
  | Word Text
  deriving (Show, Eq, Ord)

instance IsString Shape where
  fromString w = Word (Text.pack w)

type Pattern = Seq1 Shape

makePattern :: NonEmpty Shape -> Pattern
makePattern = Seq1.fromNonEmpty



-- INVARIANT: Map in MustGo must be nonempty.

data PatternDecision
  = MustGo (Map Shape PatternDecision)
  | MayStop (Map Shape PatternDecision)
  deriving (Show, Eq, Ord)

--

type Patterns = Map Shape PatternDecision

fromPattern :: Pattern -> Patterns
fromPattern = \case
  shape :<|| IsEmpty ->
    Map.singleton shape (MayStop mempty)
  shape :<|| (IsSeq1 pat) ->
    Map.singleton shape (MustGo (fromPattern pat))

insertPattern :: Pattern -> Patterns -> Patterns
insertPattern pat@(head :<|| tail) pats = case Map.lookup head pats of
  Nothing -> fromPattern pat <> pats
  Just _dec -> case tail of
    IsEmpty -> Map.adjust mayStop head pats
    IsSeq1 pat' -> Map.adjust (merge pat') head pats

mayStop :: PatternDecision -> PatternDecision
mayStop = \case
  MustGo  pats -> MayStop pats
  MayStop pats -> MayStop pats

merge :: Pattern -> PatternDecision -> PatternDecision
merge pat = \case
  MustGo  pats -> MustGo  (insertPattern pat pats)
  MayStop pats -> MayStop (insertPattern pat pats)


fromPatterns :: [Pattern] -> Patterns
fromPatterns [] = mempty
fromPatterns [pat] = fromPattern pat
fromPatterns (pat:pats) = insertPattern pat (fromPatterns pats)

pat1, pat2 :: Pattern
pat1 = Seq1.fromNonEmpty ["natural", "number"]
pat2 = Seq1.fromNonEmpty ["natural"]
pat3 = Seq1.fromNonEmpty ["natural", "isomorphism"]
pat4 = Seq1.fromNonEmpty ["integer"]

pats1, pats2 :: Patterns
pats1 = fromPattern pat1
pats2 = insertPattern pat2 pats1
pats3 = insertPattern pat3 pats1
pats4 = insertPattern pat4 pats1
