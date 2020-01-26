module Parse.Declaration where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Statement (Statement, statement)
import Parse.Token (environment, period, word)
import Parse.Definition (Definition, definition)


data Declaration
   = DeclAxiom Axiom
   | DeclDefinition Definition
   | DeclTheorem Theorem
   deriving (Show, Eq)

declaration :: Parser Declaration
declaration = DeclAxiom <$> axiom
   <|> DeclDefinition <$> definition
   <|> DeclTheorem <$> theorem


data Axiom = Axiom
   { assumptions :: ![Assumption]
   , content :: !Statement
   } deriving (Show, Eq)

axiom :: Parser Axiom
axiom = environment "axiom" do
   asms <- many assumption
   optional (word "then")
   stmt <- statement `endedBy` period
   return (Axiom asms stmt)


data Theorem = Theorem
   { theoremAssumptions :: ![Assumption]
   , theoremStatement :: !Statement
   } deriving (Show, Eq)

theorem :: Parser Theorem
theorem = environment "theorem" do
   asms <- many assumption
   optional (word "then")
   thm <- statement `endedBy` period
   return (Theorem asms thm)
