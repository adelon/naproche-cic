module Parse.Statement where


import Base.Parser
import Language.Common (Var)
import Language.Expression
import Parse.Expression

import Data.Text (Text)


type Adj = Text

data Statement
  = Var `Is` Adj
  | Negated Statement
  | IfThen Statement Statement
  | PropStatement Prop
  deriving (Show, Eq)

statement :: Parser Statement
statement = negated

negated :: Parser Statement
negated = Negated <$> do
  notTheCase
  p <- Squashed <$> math expr
  period
  return (PropStatement p)
  where
    notTheCase = word "it" *> word "is" *> word "not" *> word "the" *> word "case" *> word "that"