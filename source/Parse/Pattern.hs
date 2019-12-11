module Parse.Pattern where

import Base.Parser
import Language.Pattern (Pattern, Patterns, Shape(..))
import Parse.Token (word)

import Data.Sequence1 as Seq1


-- Also returns the pattern that succeeded.
patterns :: Parser a -> Patterns -> Parser (Pattern, [a])
patterns _slot [] = fail "no patterns start with this word"
patterns slot (Node Slot [] : pats) = go <|> patterns slot pats
  where
    go = do
      a <- try slot
      return (Seq1.singleton Slot, [a])
patterns slot (Node w@(Word ws) [] : pats) = go <|> patterns slot pats
  where
    go = do
      asum (word <$> ws)
      return (Seq1.singleton w, [])
patterns slot (pat : pats) = pattern slot pat <|> patterns slot pats

pattern :: Parser a -> Tree Shape -> Parser (Pattern, [a])
pattern slot (Node Slot pats) = do
  a <- try slot
  (\(pat, as) -> (pat `Seq1.snoc` Slot, a : as)) <$> patterns slot pats
pattern slot (Node (Word ws) pats) = do
  asum (word <$> ws)
  (\(pat, as) -> (pat `Seq1.snoc` (Word ws), as)) <$> patterns slot pats