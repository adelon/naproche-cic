module Parse.Pattern where

import Base.Parser
import Language.Common (Var)
import Language.Pattern (Pattern(..), Shape(..))
import Parse.Var (var)

import Data.Foldable (asum)
import Data.Functor ((<$))


-- | Parses a pattern. Success is indicated by returning a list
 -- of the variables used in the slots of the pattern.
pattern :: Pattern -> Parser [Var]
pattern (Pattern []) = return []
pattern (Pattern (s:pat)) = do
  mv <- shape s
  case mv of
    Nothing -> pattern (Pattern pat)
    Just v -> (v :) <$> pattern (Pattern pat)

-- | Parses a shape. Success is indicated by returning @Nothing@
-- for words and @Just Var@ for variables filling a slot.
shape :: Shape -> Parser (Maybe Var)
shape = \case
  Word w -> Nothing <$ asum (word <$> w)
  Slot -> Just <$> var