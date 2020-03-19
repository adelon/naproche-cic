module Parse.Declaration where


import Base
import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Statement (Statement, statement)
import Parse.Token
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
   { axiomTag :: Maybe Text
   , axiomAssumptions :: ![Assumption]
   , axiomStatement :: !Statement
   } deriving (Show, Eq)

axiom :: Parser Axiom
axiom = environment "axiom" do
   tag <- optional environmentTag
   asms <- many assumption
   optional (word "then")
   stmt <- statement `endedBy` period
   pure (Axiom tag asms stmt)


data Theorem = Theorem
   { theoremTag :: Maybe Text
   , theoremAssumptions :: ![Assumption]
   , theoremStatement :: !Statement
   } deriving (Show, Eq)

theorem :: Parser Theorem
theorem = environment "theorem" do
   tag <- optional environmentTag
   asms <- many assumption
   optional (word "then")
   thm <- statement `endedBy` period
   pure (Theorem tag asms thm)


newtype Remark = Remark [Tok] deriving (Show, Eq)

remark :: Parser Remark
remark = environment "remark" do
   toks <- many (anyTokenBut (Set.singleton (EndEnv "remark")))
   pure (Remark toks)
