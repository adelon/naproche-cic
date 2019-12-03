module Language.Pattern (Pattern(..), Shape(..)) where


import Data.Text (Text)


data Shape
  = Slot
  | Word [Text] -- List of acceptable synonyms
  deriving (Show, Eq)

newtype Pattern = Pattern { unPattern :: [Shape]}
  deriving (Show, Eq, Semigroup, Monoid)