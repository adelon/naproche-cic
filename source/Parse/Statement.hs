module Parse.Statement where


import Language.Common (Var)
import Base.Parser

import Data.Text (Text)


type Adj = Text

data Statement
  = Var `Is` Adj
  | Negated Statement
  | IfThen Statement Statement
  deriving (Show, Eq)

statement :: Parser Statement
statement = undefined