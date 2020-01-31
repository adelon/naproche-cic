module Language.Pattern
  ( Shape(..)
  , Pattern
  , Patterns
  , makePattern
  , fromPattern, fromPatterns
  , insertPattern
  , interpretPattern
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
type Interpretation = ()
type Pattern = [Shape]

makePattern :: [Shape] -> Pattern
makePattern = id

-- |
-- Singleton pattern.
fromPattern :: Pattern -> Patterns
fromPattern pat = Trie.singleton pat ()

-- TODO: Add interpretation instead of ().
fromPatterns :: [Pattern] -> Patterns
fromPatterns pats = Trie.fromList
   ((\pat -> (pat, ())) <$> pats)

insertPattern :: Pattern -> Patterns -> Patterns
insertPattern pat = Trie.insert pat ()

interpretPattern :: Pattern -> Patterns -> Maybe ()
interpretPattern = Trie.lookup
