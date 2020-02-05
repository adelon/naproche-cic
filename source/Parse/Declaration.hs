module Parse.Declaration where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Statement (Statement, statement)
import Parse.Token (Tok(EndEnv), environment, period, word, anyTokenBut)
import Parse.Definition (Definition, definition)

import qualified  Data.Set as Set


data Declaration
   = DeclAxiom Axiom
   | DeclDefinition Definition
   | DeclTheorem Theorem
   | DeclRemark Remark
   deriving (Show, Eq)

declaration :: Parser Declaration
declaration = DeclAxiom <$> axiom
   <|> DeclDefinition <$> definition
   <|> DeclTheorem <$> theorem
   <|> DeclRemark <$> remark

data Axiom = Axiom
   { assumptions :: ![Assumption]
   , content :: !Statement
   } deriving (Show, Eq)

axiom :: Parser Axiom
axiom = environment "axiom" do
   asms <- many assumption
   optional (word "then")
   stmt <- statement `endedBy` period
   pure (Axiom asms stmt)


data Theorem = Theorem
   { theoremAssumptions :: ![Assumption]
   , theoremStatement :: !Statement
   } deriving (Show, Eq)

theorem :: Parser Theorem
theorem = environment "theorem" do
   asms <- many assumption
   optional (word "then")
   thm <- statement `endedBy` period
   pure (Theorem asms thm)


newtype Remark = Remark [Tok] deriving (Show, Eq)

remark :: Parser Remark
remark = environment "remark" do
   toks <- many (anyTokenBut (Set.singleton (EndEnv "remark")))
   pure (Remark toks)
