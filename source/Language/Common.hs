module Language.Common where


import Data.Text.Prettyprint.Doc


newtype Var = Var Text deriving (Eq, Ord, IsString)

instance Show Var where
   show (Var v) = show v

instance Pretty Var where
   pretty (Var v) = pretty v
