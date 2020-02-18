module Language.Pattern
  ( Shape(..)
  , Pattern
  , Patterns
  , makePattern
  , fromPattern, fromPatterns
  , insertPattern
  , interpretPattern
  , makeInterpretation
  ) where


import Data.Text.Prettyprint.Doc
import Data.Trie.Map (TMap)

import qualified Data.Text as Text
import qualified Data.Trie.Map as Trie


data Shape
  = Slot
  | Word Text
  deriving (Show, Eq, Ord)

instance IsString Shape where
  fromString w = Word (Text.pack w)

---instance Pretty Pattern where
--  pretty :: forall ann. Pattern -> Doc ann
--  pretty shapes = pretty $ Text.dropWhileEnd (== '_') $ foldr1 (<>) (prettyShape <$> shapes)

instance Pretty Shape where
  pretty :: forall ann. Shape -> Doc ann
  pretty Slot = "_" -- "<?>"
  pretty (Word w) = pretty w <> ""


type Patterns = TMap Shape Interpretation
type Pattern = [Shape]

-- An interpretation of a pattern is the corresponding Lean function.
-- They are typically determined from the shapes of the pattern, but
-- some built-in patterns may map to functions from stdlib or mathlib.
--
type Interpretation = Text

makePattern :: [Shape] -> Pattern
makePattern = id

-- |
-- Singleton pattern.
fromPattern :: Pattern -> Interpretation -> Patterns
fromPattern pat interpr = Trie.singleton pat interpr

-- TODO: Add interpretation instead of ().
fromPatterns :: [(Pattern, Interpretation)] -> Patterns
fromPatterns = Trie.fromList

insertPattern :: Pattern -> Interpretation -> Patterns -> Patterns
insertPattern pat interpr = Trie.insert pat interpr

interpretPattern :: Pattern -> Patterns -> Maybe Text
interpretPattern = Trie.lookup

makeInterpretation :: Pattern -> Interpretation
makeInterpretation = \case
  [] -> "_"
  Slot : pat -> "__" <> makeInterpretation pat
  Word w : pat -> w <> "_" <> makeInterpretation pat
