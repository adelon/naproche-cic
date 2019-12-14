module Parse.Definition where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Pattern (Pattern)
import Parse.Statement (Statement, statement)
import Parse.Token (environment, word, iff)
import Parse.Var (Var, varList)

data Definition
  = Definition [Assumption] DefinitionBody
  deriving (Show, Eq)

definition :: Parser Definition
definition = environment "definition" do
  asms <- many1 assumption
  optional weSay
  return (Definition asms undefined)
    where
      weSay :: Parser ()
      weSay = void (try (word "we" >> word "say") >> optional (word "that"))

data DefinitionBody
  = DefinePredicate PredicateHead Statement
  deriving (Show, Eq)

definitionBody :: Parser DefinitionBody
definitionBody = do
  optional weSay
  head <- predicateHead
  iff
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
predicateHead = do
  vars <- varList
  let pat = error "Parse.Definition.predicateHead incomplete"
  return (PredicateAdjPattern vars pat)
