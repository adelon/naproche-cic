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
{-
    PatternContinue w@(Word ws) patContinues : otherPats ->
      | patContinues /= Set.singleton PatternEnd
        -> go <|> patterns slot (Set.fromDescList otherPats)
          where
            go = do
              asum (word <$> ws)
              (\(pat, as) -> (pat `Seq1.snoc` (Word ws), as)) <$> patterns slot patContinues

    PatternContinue Slot patContinues : otherPats -> go <|> patterns slot (Set.fromDescList otherPats)
      where
        go = case patContinues == Set.singleton PatternEnd of
          True -> do
            a <- try slot
            return (Seq1.singleton Slot, [a])
          False -> do
            a <- try slot
            (\(pat, as) -> (pat `Seq1.snoc` Slot, a : as)) <$> patterns slot patContinues
    [] -> fail "no patterns start with this word"
-}
{-
  go = case patContinues == Set.singleton PatternEnd of
              True -> do
                asum (word <$> ws)
                return (Seq1.singleton w, [])
              False -> do
                asum (word <$> ws)
                (\(pat, as) -> (pat `Seq1.snoc` (Word ws), as)) <$> patterns slot patContinues

          -}