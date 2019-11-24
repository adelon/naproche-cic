module Parse.Assumption where


import Base.Parser
import Language.Expression
import Parse.Expression (typing)
import Parse.Statement (Statement, statement)


data Assumption
  = AssumptionPretyping (Typing Var Typ)
  | Assumption Statement

assumption :: Parser Assumption
assumption = pretyping <|> Assumption <$> statement

pretyping :: Parser Assumption
pretyping = do
  word "let"
  info <- math typing
  period
  return (AssumptionPretyping info)