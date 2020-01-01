module Parse.Document where


import Base.Parser (Parser, many1)

import Parse.Declaration


newtype Document = Document (NonEmpty Declaration) deriving (Show)


document :: Parser Document
document = Document <$> many1 declaration
