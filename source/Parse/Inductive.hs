module Parse.Inductive where


import Base
import Base.Parser (Parser, sepBy1)
import Parse.Token (command, symbol, environment, grouped)


data InferRule = InferRule
  { inferPremises :: NonEmpty ()
  , inferConclusion :: ()
  }


-- Uses the concrete syntax for inference rules from 'proof.sty' for LaTeX. E.g.:
--
-- \begin{inductive}
--    \infer{\succ{n}\in ℕ}{n\in ℕ}
-- \end{inductive}
--
inductive :: Parser InferRule
inductive = environment "inductive" do
  command "infer"
  concl <- grouped conclusion
  prems <- grouped premises
  pure (InferRule prems concl)
  where
    conclusion = pure ()
    premises = pure () `sepBy1` symbol "&"


-- TODO
--
-- It may be better to have specialized environments for various
-- inductive definitions; 'inductiverel', 'inductiveprop' etc.





-- TODO
--
-- We would also like to include support for mutual inductive types
-- and patterns in inductive definitions. The following is as a first
-- sketch of a possible phrasing for this.
--
--
-- Definition. (In a "mutualinductive" environment)
--
-- Define n being odd and n being even as follows.
--
--                 n is odd       n is even
--   ---------   -------------   ------------
--   0 is even   n + 1 is even   n + 1 is odd
--
--
--
--
-- One more possible phrasing:
--
--  *  0 is even.
--  *  if n is odd, then n + 1 is even.
--  *  if n is even, then n + 1 is odd.
