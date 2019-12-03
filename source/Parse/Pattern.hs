module Parse.Pattern where

import Base.Parser
import Language.Common (Var)
import Language.Pattern (Pattern(..), Shape(..))
import Parse.Var (var)

import Data.Foldable (asum)
import Data.Functor ((<$))


pattern :: Pattern -> Parser [Var]
pattern (Pattern []) = return []
pattern (Pattern (s:pat)) = do
  mv <- shape s
  case mv of
    Nothing -> pattern (Pattern pat)
    Just v -> (v :) <$> pattern (Pattern pat)


shape :: Shape -> Parser (Maybe Var)
shape = \case
  Word w -> Nothing <$ asum (word <$> w)
  Slot -> Just <$> var