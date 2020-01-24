module Parse.Pattern
   ( module Parse.Pattern
   , module Language.Pattern
   )
   where


import Base.Parser
import Language.Pattern
import Parse.Token (math, word, anyWord, anyWordBut)
import Parse.Var (Var, var)
import Parse.Expression (Typ, varInfo)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence1 as Seq1
import qualified Data.Map as Map


-- Parses a generic pattern with variables in slots until
-- the `stop` parser succeeds. Returns the shapes of the pattern
-- and the list of variables filling the slots of the pattern.
-- The results needs to validated to create a proper pattern.
anyPatternTill :: forall stop. Parser stop -> Parser (Pattern, [Var])
-- TODO: This is a rather silly placeholder implementation.
-- Should be fixed once the rest of the parsing setup works.
-- Use many1Till instead, which is exported from the parser module and
-- returns a nonempty list instead.
anyPatternTill stop = do
   (shapes, vars) <- concatUnzip <$> someTill (wordShape <|> slotShape) stop
   let pat = makePattern (NonEmpty.fromList shapes)
   return (pat, vars)
   where
      wordShape :: Parser ([Shape], [Var])
      wordShape = do
         w <- anyWord
         return ([Word w], [])
      slotShape :: Parser ([Shape], [Var])
      slotShape = do
         v <- math var
         return ([Slot], [v])
      concatUnzip :: [([a],[b])] -> ([a], [b])
      concatUnzip asbs = (concat *** concat) (unzip asbs)

anyPatternBut :: Set Text -> Parser (Pattern, [(Var, Maybe Typ)])
anyPatternBut buts = do
   (shapes, vars) <- concatUnzip <$> some (wordShape <|> slotShape)
   let pat = makePattern (NonEmpty.fromList shapes)
   return (pat, vars)
   where
      wordShape :: Parser ([Shape], [(Var, Maybe Typ)])
      wordShape = do
         w <- anyWordBut buts
         return ([Word w], [])
      slotShape :: Parser ([Shape], [(Var, Maybe Typ)])
      slotShape = do
         v <- math varInfo
         return ([Slot], [v])
      concatUnzip :: [([a],[b])] -> ([a], [b])
      concatUnzip asbs = (concat *** concat) (unzip asbs)

patternWith :: forall a. Parser a -> Patterns -> Parser (Pattern, [a])
patternWith slot pats = makeProperPattern <$> patterns_ slot pats
   where
      makeProperPattern (mpat, as) = case mpat of
         Just pat -> (pat, as)
         Nothing -> error "Parse.Pattern.patterns has parsed a pattern incorrectly, resulting in an empty pattern"

consWord :: forall a. Text -> (Maybe Pattern, [a]) -> (Maybe Pattern, [a])
consWord w (mpat, as) = case mpat of
   Nothing -> (Just (Seq1.singleton (Word w)), as)
   Just pat -> (Just ((Word w) `Seq1.cons` pat), as)

consSlot :: forall a. a -> (Maybe Pattern, [a]) -> (Maybe Pattern, [a])
consSlot a (mpat, as) = case mpat of
   Nothing -> (Just (Seq1.singleton Slot), a : as)
   Just pat -> (Just (Slot `Seq1.cons` pat), a : as)

patterns_ :: forall a. Parser a -> Patterns -> Parser (Maybe Pattern, [a])
patterns_ slot pats = case Map.keys pats of
   []     -> fail "no such pattern"
   shapes -> do
      wordOrSlot <- asum (shape_ slot <$> shapes)
      case wordOrSlot of
         Left w -> consWord w <$> patterns__ slot ((Map.!) pats (Word w))
         Right a -> consSlot a <$> patterns__ slot ((Map.!) pats Slot)

patterns__ :: Parser a -> PatternDecision -> Parser (Maybe Pattern, [a])
patterns__ slot = \case
   MustGo  pats -> patterns_ slot pats
   MayStop pats -> patterns_ slot pats <|> pure (Nothing, [])


shape_ :: forall a. Parser a -> Shape -> Parser (Either Text a)
shape_ _  (Word w) = Left  <$> (word w >> pure w)
shape_ slot (Slot) = Right <$> slot
