module Parse.Inductive where


data InferRule = InferRule
  { inferPremises :: ()
  , inferConclusion :: ()
  }




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
