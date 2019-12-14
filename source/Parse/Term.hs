module Parse.Term where


import Language.Pattern
import Language.Quantifier


data Term
  -- Definite terms.
  = TermPattern Pattern [Term]
  -- Indefinite terms.
  | TermQuantifiedNominal Quantifier Pattern [Term]