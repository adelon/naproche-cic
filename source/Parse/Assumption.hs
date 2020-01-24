module Parse.Assumption where


import Base.Parser
import Language.Expression
import Parse.Expression (typing)
import Parse.Statement
import Parse.Token


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
