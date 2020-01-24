module Language.Quantifier where

import Data.Text.Prettyprint.Doc (Doc, Pretty(..))


data Quantifier
   = Universal
   | AlmostUniversal
   | Existential
   | UniqueExistential
   | Nonexistential
   deriving (Show, Eq, Ord)

instance Pretty Quantifier where
   pretty :: forall ann. Quantifier -> Doc ann
   pretty = \case
      Universal           -> "∀"
      AlmostUniversal     -> "∀∞"
      Existential         -> "∃"
      UniqueExistential   -> "∃!"
      Nonexistential      -> "∄"
