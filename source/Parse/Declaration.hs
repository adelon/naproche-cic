module Parse.Declaration where


import Base.Parser


data Declaration
  = DeclAxiom Axiom
  | DeclDefinition Definition
  | DeclTheorem Theorem

decl :: Parser Declaration
decl = DeclAxiom <$> axiom
  <|> DeclDefinition <$> definition
  <|> DeclTheorem <$> theorem


data Axiom

axiom :: Parser Axiom
axiom = environment "axiom" do
  error "Declaration.axiom unfinished"


data Definition

definition :: Parser Definition
definition = environment "definition" do
  error "Declaration.defn unfinished"


data Theorem

theorem :: Parser Theorem
theorem = environments ["theorem", "lemma", "proposition"] do
  error "Declaration.theorem unfinished"