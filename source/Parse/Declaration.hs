module Parse.Declaration where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Statement (Statement, statement)


data Declaration
  = DeclAxiom Axiom
  | DeclDefinition Definition
  | DeclTheorem Theorem

decl :: Parser Declaration
decl = DeclAxiom <$> axiom
  <|> DeclDefinition <$> definition
  <|> DeclTheorem <$> theorem


data Axiom = Axiom
  { assumptions :: [Assumption]
  , content :: Statement
  }

axiom :: Parser Axiom
axiom = environment "axiom" do
  asms <- many1 assumption
  stmt <- statement
  return (Axiom asms stmt)


data Definition

definition :: Parser Definition
definition = environment "definition" do
  error "Declaration.defn unfinished"


data Theorem

theorem :: Parser Theorem
theorem = environments ["theorem", "lemma", "proposition"] do
  error "Declaration.theorem unfinished"