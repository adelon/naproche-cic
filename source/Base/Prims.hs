-- This module describes all the built-in definitions (primitives).
-- We map nouns, verb, adjectives and symbolic expressions to their
-- respective interpretation in a target language. We store pattern
-- interpretations in the 'Patterns' trie data type, which allows both
-- 'Map'-like lookup and generation of effective parsers.
--
-- In the future, the data described here could be specified by
-- an external file, allowing users to modify the primitives.
--
-- We give Lean specific interpretations here, but in theory this
-- could be generalized to accommodate other formalization systems.


module Base.Prims where


import Base
import Language.Expression (Expr(..), Prop(..))
import Language.Pattern
import Parse.Token (Tok(..))

import qualified Data.Map.Strict as Map


primNominals :: Patterns
primNominals = fromPatterns
   [ (makePattern ["natural", "number"], "ℕ")
   , (makePattern ["rational", "number"], "ℚ") -- MathLib `data.rat.basic`.
   ]

primVerbs :: Patterns
primVerbs = fromPatterns
   [ (makePattern ["divides", Slot], "has_dvd.dvd")
   ]

primDistributiveAdjs :: Patterns
primDistributiveAdjs = fromPatterns
   [ (makePattern ["even"], "even")
   , (makePattern ["odd"], "odd")
   , (makePattern ["associative"], "associative")
   ]


primRelators :: Map Tok Text
primRelators = Map.fromList
   [ (Symbol "=", "eq" )
   , (Command "neq", "ne")
   , (Symbol ">", "gt" )
   , (Symbol "<", "has_lt.lt")
   , (Command "geq", "ge")
   , (Command "leq", "has_le.le")
   , (Command "mid", "has_dvd.dvd")
   ]

applyRelator :: Text -> Expr -> Expr -> Prop
applyRelator rel = \x y -> Predicate rel `PredApp` x `PredApp` y


primFuns :: Map Tok Text
primFuns = Map.fromList
   [ (Command "addNat", "nat.add")
   , (Command "mulNat", "nat.mul")
   , (Command "add", "add")
   , (Command "mul", "mul")
--
-- n choose k (defined in data/nat/basic.lean)
   , (Command "binom", "nat.choose")
--
-- factorial  (defined in data/nat/basic.lean)
   , (Command "fact", "nat.fact")
--
-- distance   (defined in data/nat/dist.lean)
   , (Command "distNat", "nat.dist")
--
-- fraction   (defined in data.rat.basic.lean)
   , (Command "frac", "rat.mk")
   ]
