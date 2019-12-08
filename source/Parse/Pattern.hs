module Parse.Pattern where

import Base.Parser
import Language.Pattern (Pattern(..), Shape(..))
import Parse.Token (word, anyWord)

import Data.Foldable (asum)


-- | Parses a pattern. Success is indicated by returning a list
-- of the results of the parser used for slots of the pattern.
pattern :: Parser a -> Pattern -> Parser [a]
pattern _p (Pattern []) = return []
pattern p (Pattern (s : s' : pat)) = do
  case s of
    Word w -> case s' of
      -- This is intended to disambiguate things like
      -- "x converges" and "x converges to y".
      Slot -> do
        try (asum (word <$> w) >> never anyWord) >> pattern p (Pattern (s' : pat))
      Word _w -> do
        asum (word <$> w) >> pattern p (Pattern (s' : pat))
    Slot -> do
      r <- p
      (r :) <$> pattern p (Pattern (s' : pat))
pattern p (Pattern (s : pat)) = do
  case s of
    Word w -> asum (word <$> w) >> pattern p (Pattern pat)
    Slot -> do
      r <- p
      (r :) <$> pattern p (Pattern pat)
