module Parse.Pattern where

import Base.Parser
import Language.Pattern (Pattern, Shape(..))
import Parse.Token (word, anyWord)

import Data.Foldable (asum)
import Data.Set (Set)
import Data.List.NonEmpty as NonEmpty

import qualified Data.Set as Set

-- Also returns the pattern that succeeded.
patterns :: Parser a -> Set Pattern -> Parser (Pattern, [a])
patterns slot pats = asum $ try . patternMarked slot <$> Set.toDescList pats

patternMarked :: Parser a -> Pattern -> Parser (Pattern, [a])
patternMarked slot pat = (\x -> (pat, x)) <$> pattern slot pat

-- | Parses a pattern. Success is indicated by returning a list
-- of the results of the parser used for slots of the pattern.
pattern :: Parser a -> Pattern -> Parser [a]
pattern slot (s :| (s' : pat)) = do
  case s of
    Word w -> case s' of
      -- This is intended to disambiguate things like
      -- "x converges" and "x converges to y".
      Slot -> do
        try (asum (word <$> w) >> never anyWord)
        pattern slot (s' :| pat)
      Word _w -> do
        asum (word <$> w)
        pattern slot (s' :| pat)
    Slot -> do
      r <- slot
      (r :) <$> pattern slot (s' :| pat)
pattern slot (s :| []) = do
  case s of
    Word w -> asum (word <$> w) >> return []
    Slot -> do
      r <- slot
      return (pure r)
