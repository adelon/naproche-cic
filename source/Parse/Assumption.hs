module Parse.Assumption where


import Base.Parser
import Parse.Expression (Typing, Var, Typ, typing)
import Parse.Statement (Statement, statement)
import Parse.Token (word, math, period)


data Assumption
   = AssumptionPretyping (NonEmpty (Typing Var Typ))
   | Assumption Statement
   deriving (Show, Eq)

assumption :: Parser Assumption
assumption = pretyping <|> suppose

suppose :: Parser Assumption
suppose = Assumption <$> do
   word "suppose"
   stmt <- statement
   period
   return stmt

pretyping :: Parser Assumption
pretyping = do
   word "let"
   asm <- math typing
   period
   return (AssumptionPretyping asm)
