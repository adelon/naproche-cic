module Parse.Statement where


import Base.Parser
import Language.Common (Var)
import Language.Expression
import Parse.Expression
import Parse.Var

import Data.Text (Text)


type Adj = Text

data Statement
  = Var `Is` Adj
  | Negated Statement
  | IfThen Statement Statement
  | PropStatement Prop
  deriving (Show, Eq)

statement :: Parser Statement
statement = prop <|> negated <|> is

is :: Parser Statement
is = do
  v <- math var
  word "is"
  adj <- adjective
  return (v `Is` adj)

adjective = undefined

prop :: Parser Statement
prop = PropStatement <$> do
  optional (word "then")
  p <- Squashed <$> math expr
  period
  return p

negated :: Parser Statement
negated = Negated <$> do
  notTheCase
  p <- Squashed <$> math expr
  period
  return (PropStatement p)
  where
    notTheCase = word "it" *> word "is" *> word "not" *> word "the" *> word "case" *> word "that"