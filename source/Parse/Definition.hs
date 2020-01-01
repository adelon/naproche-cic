module Parse.Definition where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Expression (Typ, varInfo)
import Parse.Pattern (Pattern, anyPatternBut)
import Parse.Statement (Statement, statement)
import Parse.Token (Tok(..), environment, math, word, iff, period)
import Parse.Var (Var)

import qualified Data.Set as Set


data Definition
  = Definition [Assumption] DefinitionBody
  deriving (Show, Eq)

definition :: Parser Definition
definition = environment "definition" do
  asms <- many assumption
  body <- definitionBody
  return (Definition asms body)

data DefinitionBody
  = DefinePredicate PredicateHead Statement
  deriving (Show, Eq)

definitionBody :: Parser DefinitionBody
definitionBody = do
  optional weSay
  head <- predicateHead
  iff
  stmt <- statement `endedBy` period
  return (DefinePredicate head stmt)
    where
      weSay :: Parser ()
      weSay = void (try (word "we" >> word "say") >> optional (word "that"))

data PredicateHead
  = PredicateAdjPattern (NonEmpty (Var, Maybe Typ)) Pattern
  | PredicateVerbPattern (NonEmpty (Var, Maybe Typ)) Pattern
  deriving (Show, Eq)

predicateHead :: Parser PredicateHead
predicateHead = do
  -- TODO: nominals, x-of-y
  v <- math varInfo
  peeking <- optional continue
  case peeking of
    Just (Word "is") -> do
      (pat, vs) <- anyPatternBut (Set.fromList ["if", "iff"])
      registerAdj pat
      let vars = v :| vs
      return (PredicateAdjPattern vars pat)
    _otherwise -> do
      (pat, vs) <- anyPatternBut (Set.fromList ["if", "iff"])
      registerVerb pat
      let vars = v :| vs
      return (PredicateVerbPattern vars pat)
  where
    continue :: Parser Tok
    continue = word "is"
