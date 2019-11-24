module Parse.Assumption where


import Base.Parser
import Language.Expression
import Parse.Expression (typing)


data Assumption
  = Pretyping (Typing Var Typ)

assumption :: Parser Assumption
assumption = pretyping

pretyping :: Parser Assumption
pretyping = do
  word "let"
  info <- math typing
  period
  return (Pretyping info)