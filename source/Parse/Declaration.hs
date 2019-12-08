module Parse.Declaration where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Statement (Statement, statement)
import Parse.Token (environment, period)


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
  { assumptions :: ![Assumption]
  , content :: !Statement
  } deriving (Show, Eq)

axiom :: Parser Axiom
axiom = environment "axiom" do
  asms <- many assumption
  stmt <- statement `endedBy` period
  return (Axiom asms stmt)


data Definition = Definition deriving (Show, Eq)

definition :: Parser Definition
definition = environment "definition" do
  error "Declaration.definition unfinished"


data Theorem = Theorem Statement deriving (Show, Eq)

theorem :: Parser Theorem
theorem = environment "theorem" do
  thm <- statement `endedBy` period
  return (Theorem thm)
