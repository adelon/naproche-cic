module Parse.Declaration where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Statement (Statement, statement)


data Declaration
  = DeclAxiom Axiom
  | DeclDefinition Definition
  | DeclTheorem Theorem
  deriving (Show, Eq)

declaration :: Parser Declaration
declaration = DeclAxiom <$> try axiom
  <|> DeclDefinition <$> try definition
  <|> DeclTheorem <$> theorem


data Axiom = Axiom
  { assumptions :: [Assumption]
  , content :: Statement
  } deriving (Show, Eq)

axiom :: Parser Axiom
axiom = environment "axiom" do
  asms <- many1 assumption
  stmt <- statement
  return (Axiom asms stmt)


data Definition = Definition deriving (Show, Eq)

definition :: Parser Definition
definition = environment "definition" do
  error "Declaration.defn unfinished"


data Theorem = Theorem deriving (Show, Eq)

theorem :: Parser Theorem
theorem = environments ["theorem", "lemma", "proposition"] do
  error "Declaration.theorem unfinished"