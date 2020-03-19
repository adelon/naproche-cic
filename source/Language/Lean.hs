module Language.Lean where


import Base

import qualified Data.Text as Text


preamble :: Text
preamble = Text.intercalate "\n"
   [ "-- BEGIN PREAMBLE"
   , "import data.nat.basic"
   , "import data.nat.dist"
   , "import data.rat"
   , ""
   -- Defines special notation for almost-universal quantification.
   -- In the future this should be expanded to a type class for more general use.
   , "definition almost_all_nat (p : ℕ  -> Prop) := ∃ l, ∀ n : ℕ, n ≤ l → p n"
   , "notation `∀∞` binders `, ` r:(scoped P, almost_all_nat P) := r"
   , ""
   , "mutual inductive even, odd"
   , "with even : ℕ → Prop"
   , "| even_zero : even 0"
   , "| even_succ : ∀ n, odd n → even (n + 1)"
   , "with odd : ℕ → Prop"
   , "| odd_succ : ∀ n, even n → odd (n + 1)"
   , ""
   , "notation `∄` binders `, ` r:(scoped P, ¬ ∃ n, P n) := r"
   , "notation `natural_number` := ℕ"
   , "notation `rational_number` := ℚ"
   , ""
   , "axiom omitted {p : Prop} : p"
   , ""
   , "-- END PREAMBLE"
   ]
