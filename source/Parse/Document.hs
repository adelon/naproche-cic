module Parse.Document where


import Base.Parser
import Language.Expression

import Parse.Declaration
import Parse.Expression
import Parse.Instruction


newtype Document = Document [Declaration] deriving (Show)


document :: Parser Document
document = Document <$> many1 declaration
