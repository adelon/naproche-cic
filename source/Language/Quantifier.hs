module Language.Quantifier where


data Quantifier
  = Universal
  | Existential
  | Nonexistential
  deriving (Show, Eq, Ord)