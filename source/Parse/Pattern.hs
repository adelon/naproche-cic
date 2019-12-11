module Parse.Pattern where

import Base.Parser
import Language.Pattern (Pattern, Patterns, Shape(..))
import Parse.Token (word)

import Data.Sequence1 as Seq1

import qualified Data.Set as Set

-- Also returns the pattern that succeeded.
patterns :: Parser a -> Patterns -> Parser (Pattern, [a])
patterns _slot [] = return (Seq1.singleton Slot, []) -- TODO: remove this and fix the recursion calls
patterns slot (pat:[]) = pattern slot pat
patterns slot (pat:pats) = pattern slot pat <|> patterns slot pats

pattern :: Parser a -> Tree Shape -> Parser (Pattern, [a])
pattern slot (Node Slot pats) = do
  a <- try slot
  (\(pat, as) -> (pat `Seq1.snoc` Slot, a : as)) <$> patterns slot pats
pattern slot (Node (Word ws) pats) = do
  asum (word <$> ws)
  (\(pat, as) -> (pat `Seq1.snoc` (Word ws), as)) <$> patterns slot pats