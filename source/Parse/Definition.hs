module Parse.Definition where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Pattern (Pattern, anyPatternTill)
import Parse.Statement (Statement, statement)
import Parse.Token (environment, math, word, iff)
import Parse.Var (Var, var)

data Definition
  = Definition [Assumption] DefinitionBody
  deriving (Show, Eq)

definition :: Parser Definition
definition = environment "definition" do
  asms <- many1 assumption
  body <- definitionBody
  return (Definition asms body)

data DefinitionBody
  = DefinePredicate PredicateHead Statement
  deriving (Show, Eq)

definitionBody :: Parser DefinitionBody
definitionBody = do
  optional weSay
  head <- predicateHead
  stmt <- statement
  return (DefinePredicate head stmt)
    where
      weSay :: Parser ()
      weSay = void (try (word "we" >> word "say") >> optional (word "that"))

data PredicateHead
  = PredicateAdjPattern (NonEmpty Var) Pattern
  | PredicateVerbPattern
  deriving (Show, Eq)

predicateHead :: Parser PredicateHead
predicateHead = is
  where
    is = do
      v <- math var
      word "is"
      (pat, vs) <- anyPatternTill iff
      let vars = v :| vs
      return (PredicateAdjPattern vars pat)
