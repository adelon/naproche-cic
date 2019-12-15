module Language.Common where


newtype Var = Var Text deriving (Eq, Ord, IsString)

instance Show Var where
  show (Var v) = show v
