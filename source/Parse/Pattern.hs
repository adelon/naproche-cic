module Parse.Pattern where


import Data.Text (Text)


data Shape
  = Slot
  | Word [Text] -- List of acceptable synonyms
  deriving (Show, Eq)

newtype Pattern = Pattern [Shape]


