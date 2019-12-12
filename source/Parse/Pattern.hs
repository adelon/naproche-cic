module Parse.Pattern where

import Base.Parser
import Language.Pattern
import Parse.Token (word)

import qualified Data.Sequence1 as Seq1
import qualified Data.Set as Set

patterns :: Parser a -> Patterns -> Parser (Pattern, [a])
patterns slot pats = case Set.toDescList pats of
  tree : otherPats -> case tree of
    PatternContinue w@(Word ws) patContinues -> go <|> patterns slot (Set.fromDescList otherPats)
      where
        go = do
          asum (word <$> ws)
          (\(pat, as) -> (w `Seq1.cons` pat, as)) <$> patterns slot patContinues
    PatternEnd w@(Word ws) -> go <|> patterns slot (Set.fromDescList otherPats)
      where
        go = do
          asum (word <$> ws)
          return (Seq1.singleton w, [])
    PatternContinue Slot patContinues -> go <|> patterns slot (Set.fromDescList otherPats)
      where
        go = do
          a <- try slot
          (\(pat, as) -> (Slot `Seq1.cons` pat, a : as)) <$> patterns slot patContinues
    PatternEnd Slot -> go <|> patterns slot (Set.fromDescList otherPats)
      where
        go = do
          a <- try slot
          return (Seq1.singleton Slot, [a])
  [] -> fail "no patterns start with this word"