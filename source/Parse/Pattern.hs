module Parse.Pattern
  ( module Parse.Pattern
  , module Language.Pattern
  ) where


import Base.Parser
import Language.Pattern
import Parse.Expression (Typ, varInfo)
import Parse.Token (math, word, anyWord, anyWordBut)
import Parse.Var (Var, var)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set


-- Parses a generic pattern with variables in slots until
-- the `stop` parser succeeds. Returns the shapes of the pattern
-- and the list of variables filling the slots of the pattern.
-- The results needs to validated to create a proper pattern.
anyPatternTill :: Parser stop -> Parser (Pattern, [Var])
-- TODO: This is a rather silly placeholder implementation.
-- Should be fixed once the rest of the parsing setup works.
-- Use many1Till instead, which is exported from the parser module and
-- returns a nonempty list instead.
anyPatternTill stop = do
  (shapes, vars) <- concatUnzip <$> someTill (wordShape <|> slotShape) stop
  let pat = NonEmpty.fromList shapes
  return (pat, vars)
  where
    wordShape :: Parser ([Shape], [Var])
    wordShape = do
      w <- anyWord
      return ([Word [w]], [])
    slotShape :: Parser ([Shape], [Var])
    slotShape = do
      v <- math var
      return ([Slot], [v])
    concatUnzip :: [([a],[b])] -> ([a], [b])
    concatUnzip asbs = (concat *** concat) (unzip asbs)

anyPatternBut :: Set Text -> Parser (Pattern, [(Var, Maybe Typ)])
anyPatternBut buts = do
  (shapes, vars) <- concatUnzip <$> some (wordShape <|> slotShape)
  let pat = NonEmpty.fromList shapes
  return (pat, vars)
  where
    wordShape :: Parser ([Shape], [(Var, Maybe Typ)])
    wordShape = do
      w <- anyWordBut buts
      return ([Word [w]], [])
    slotShape :: Parser ([Shape], [(Var, Maybe Typ)])
    slotShape = do
      v <- math varInfo
      return ([Slot], [v])
    concatUnzip :: [([a],[b])] -> ([a], [b])
    concatUnzip asbs = (concat *** concat) (unzip asbs)

patternWith :: Parser a -> Set Pattern -> Parser (Pattern, [a])
patternWith slot pats = asum $ try . patternMarked slot <$> Set.toDescList pats

patternMarked :: Parser a -> NonEmpty Shape -> Parser (Pattern, [a])
patternMarked slot pat = (\x -> (pat, x)) <$> go slot pat

-- Parses a pattern. Success is indicated by returning a list
-- of the results of the parser used for slots of the pattern.
go :: Parser a -> Pattern -> Parser [a]
go slot (s :| (s' : pat)) = do
  case s of
    Word w -> case s' of
      -- This is intended to disambiguate things like
      -- "x converges" and "x converges to y".
      Slot -> do
        try (asum (word <$> w) >> never anyWord)
        go slot (s' :| pat)
      Word _w -> do
        asum (word <$> w)
        go slot (s' :| pat)
    Slot -> do
      r <- slot
      (r :) <$> go slot (s' :| pat)
go slot (s :| []) = do
  case s of
    Word w -> asum (word <$> w) >> return []
    Slot -> do
      r <- slot
      return (pure r)
