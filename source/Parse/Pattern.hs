module Parse.Pattern where

import Base.Parser
import Language.Pattern (Pattern(..), Shape(..))
import Parse.Token (word, anyWord)

import Data.Foldable (asum)
import Data.Set (Set)

import qualified Data.Set as Set


-- Precondition: list of patterns must be sorted in decreasing order.
patterns :: Parser a -> Set Pattern -> Parser [a]
patterns slot pats = asum $ try . pattern slot <$> Set.toDescList pats


-- | Parses a pattern. Success is indicated by returning a list
-- of the results of the parser used for slots of the pattern.
pattern :: Parser a -> Pattern -> Parser [a]
pattern _slot (Pattern []) = return []
pattern slot (Pattern (s : s' : pat)) = do
  case s of
    Word w -> case s' of
      -- This is intended to disambiguate things like
      -- "x converges" and "x converges to y".
      Slot -> do
        try (asum (word <$> w) >> never anyWord)
        pattern slot (Pattern (s' : pat))
      Word _w -> do
        asum (word <$> w)
        pattern slot (Pattern (s' : pat))
    Slot -> do
      r <- slot
      (r :) <$> pattern slot (Pattern (s' : pat))
pattern slot (Pattern (s : pat)) = do
  case s of
    Word w -> asum (word <$> w) >> pattern slot (Pattern pat)
    Slot -> do
      r <- slot
      (r :) <$> pattern slot (Pattern pat)
