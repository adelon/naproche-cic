module Parse.Assumption where


import Base.Parser
import Language.Expression
import Parse.Expression (typing)
import Parse.Statement (Statement, statement)


data Assumption
  = AssumptionPretyping (Typing Var Typ)
  | Assumption Statement
  deriving (Show, Eq)

assumption :: Parser Assumption
assumption = pretyping <|> Assumption <$> (word "suppose" *> statement)

pretyping :: Parser Assumption
pretyping = do
  word "let"
  asm <- math typing
  period
  return (AssumptionPretyping asm)