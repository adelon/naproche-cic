module Parse.Pattern
  ( module Parse.Pattern
  , module Language.Pattern
  ) where


import Base
import Base.Parser
import Language.Pattern
import Parse.Token (math, word, anyWord, anyWordBut)
import Parse.Var (Var, var)
import Parse.Expression (Typ, varInfo)

import qualified Data.Trie.Map as Trie


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
   let pat = shapes
   pure (pat, vars)
   where
      wordShape :: Parser ([Shape], [Var])
      wordShape = do
         w <- anyWord
         pure ([Word w], [])
      slotShape :: Parser ([Shape], [Var])
      slotShape = do
         v <- math var
         pure ([Slot], [v])
      concatUnzip :: [([a],[b])] -> ([a], [b])
      concatUnzip asbs = (concat *** concat) (unzip asbs)

anyPatternBut :: Set Text -> Parser (Pattern, [(Var, Maybe Typ)])
anyPatternBut buts = do
   (shapes, vars) <- concatUnzip <$> some (wordShape <|> slotShape)
   let pat = shapes
   pure (pat, vars)
   where
      wordShape :: Parser ([Shape], [(Var, Maybe Typ)])
      wordShape = do
         w <- anyWordBut buts
         pure ([Word w], [])
      slotShape :: Parser ([Shape], [(Var, Maybe Typ)])
      slotShape = do
         v <- math varInfo
         pure ([Slot], [v])
      concatUnzip :: [([a],[b])] -> ([a], [b])
      concatUnzip asbs = (concat *** concat) (unzip asbs)

patternWith :: forall a. Parser a -> Patterns -> Parser (Pattern, [a])
patternWith slot pats = split <$> patterns_ slot pats
   where
--
-- `split` ambs is equivalent to (fst <$> ambs, catMaybes . snd <$> ambs)
--
-- vvvvv
   split :: forall b c. [(b, Maybe c)] -> ([b],[c])
   split = \case
      (b, Just c) : bmcs ->
         (\(bs, cs) -> (b : bs, c : cs)) (split bmcs)
      (b, Nothing) : bmcs ->
         (\(bs, cs) -> (b : bs, cs)) (split bmcs)
      [] -> ([],[])


-- TODO
-- Write a manual parser that directly produces the desired result using
-- the folds from the library? We can skip eot in that case as well.
--
patterns_ :: forall a. Parser a -> Patterns -> Parser [(Shape, Maybe a)]
patterns_ slot pats = fst <$> Trie.toParser (shape_ slot) eot pats
--                    ^^^^^^^
--                    We only care about which pattern succeeded.
--
--                    In theory we could also interpret the pattern at this point,
--                    using the second value. Is that worth it?
--
   where
      eot = noop
--    ^^^
--    This should be the parser that is required at the end of a pattern
--    for the whole pattern to succeed. In our case we require no special
--    end marker for patterns and thus we use `noop = pure ()`.


shape_ :: forall a. Parser a -> Shape -> Parser (Shape, Maybe a)
shape_ _ (Word w) = word w *> pure (Word w, Nothing)
shape_ slot Slot  = (\a -> (Slot, Just a)) <$> slot
