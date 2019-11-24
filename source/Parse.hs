module Parse where


import Base.Parser
import Language.Expression

import Parse.Expression (expr)


document :: Parser Expr
document = expr
