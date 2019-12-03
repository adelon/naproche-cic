module Language.Common where


import Data.Text (Text)
import Data.String (IsString)


newtype Var = Var Text deriving (Eq, Ord, IsString)

instance Show Var where
  show (Var v) = show v