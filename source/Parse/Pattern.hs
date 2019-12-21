module Parse.Pattern
  ( Pattern
  , Patterns
  , Shape(..)
  , PatternTree(..)
  , anyPatternTill
  , patternWith
  ) where


import Base.Parser
import Language.Pattern
import Parse.Token (math, word, anyWord)
import Parse.Var (Var, var)

import qualified Data.Sequence1 as Seq1
import qualified Data.Set1 as Set1
import qualified Data.Set as Set

-- Parses a generic pattern with variables in slots until
-- the `stop` parser succeeds. Returns the shapes of the pattern
-- and the list of variables filling the slots of the pattern.
-- The results needs to validated to create a proper pattern.
anyPatternTill :: Parser stop -> Parser ([Shape], [Var])
-- TODO: This is a rather silly placeholder implementation.
-- Should be fixed once the rest of the parsing setup works.
anyPatternTill stop = concatUnzip <$> someTill (wordShape <|> slotShape) stop
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


-- TODO: The implementation of `patternWith` is rather ugly. At some point
-- it should be refactored to remove the impossible branches while
-- still keeping the data types Pattern and Patterns correct by definition.

-- `patternWith slot pats` parses one of the patterns described in `pats`
-- using the `slot` parser for the slots of patterns. This parser commits after
-- successfully parsing a word that occurs in a pattern. The result is a tuple of
-- the pattern that succeeded and the list of results of the slot parser.
patternWith :: Parser a -> Patterns -> Parser (Pattern, [a])
patternWith slot pats = makeProperPattern <$> patterns' slot pats
  where
    makeProperPattern (mpat, as) = case mpat of
      Just pat -> (pat, as)
      Nothing -> error "Parse.Pattern.patterns has parsed a pattern incorrectly, resulting in an empty pattern"

consWord :: Shape -> (Maybe Pattern, [a]) -> (Maybe Pattern, [a])
consWord w (mpat, as) = case mpat of
  Nothing -> (Just (Seq1.singleton w), as)
  Just pat -> (Just (w `Seq1.cons` pat), as)

consSlot :: a -> (Maybe Pattern, [a]) -> (Maybe Pattern, [a])
consSlot a (mpat, as) = case mpat of
  Nothing -> (Just (Seq1.singleton Slot), a : as)
  Just pat -> (Just (Slot `Seq1.cons` pat), a : as)

patterns' :: Parser a -> Patterns -> Parser (Maybe Pattern, [a])
patterns' slot pats = case Set1.toDescNonEmpty pats of
  PatternEnd :|[] -> fail "no such pattern"
  tree :| otherPats -> case tree of
    PatternContinue w@(Word ws) patContinues -> case otherPats of
      [PatternEnd] -> go <|> return (Nothing, [])
        where
          go = trace (show ws <> " at End. Other patterns: " <> show otherPats) do
            asum (word <$> ws)
            return (Just (Seq1.singleton w), [])
      _ -> go <|> patterns' slot (Set1.insertSmallest PatternEnd (Set.fromDescList otherPats))
        where
          go = trace (show ws <> "at Continue Other patterns: " <> show otherPats) do
            asum (word <$> ws)
            consWord w <$> patterns' slot patContinues
    PatternContinue Slot patContinues -> case otherPats of
      [PatternEnd] -> go <|> return (Nothing, [])
        where
          go = trace ("Slot at End Other patterns: " <> show otherPats) do
            a <- try slot
            return (Just (Seq1.singleton Slot), [a])
      _ -> go <|> patterns' slot (Set1.insertSmallest PatternEnd (Set.fromDescList otherPats))
        where
          go = trace ("Slot at Continue Other patterns: " <> show otherPats) do
            a <- try slot
            consSlot a <$> patterns' slot patContinues
    PatternEnd -> error "Parse.Pattern.patterns reached impossible branch for PatternEnd"
