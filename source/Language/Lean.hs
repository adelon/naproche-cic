module Language.Lean where


import qualified Data.Text as Text


preamble :: Text
preamble = Text.intercalate "\n\n"
   [ "axiom omitted {p : Prop} : p"
   -- Defines special notation for almost-universal quantification.
   -- In the future this should be expanded to a type class for more general use.
   , "definition almost_all_nat (p : ℕ  -> Prop) := ∃ l, ∀ n : ℕ, n ≤ l → p n"
   , "notation `∀∞` binders `, ` r:(scoped P, almost_all_nat P) := r"
   ]
