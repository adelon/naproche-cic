module Parse.Var where


import Base.Parser
import Language.Common (Var(..))

import Data.Text (Text)
import Data.Foldable (asum)


var :: Parser Var
var = Var <$> (letter <|> try greek <|> bb)

greek :: Parser Text
greek = do
  exact "\\"
  l <- asum (makeSymbolParser <$> greeks)
  return l

greeks :: [(Text,Text)]
greeks =
  [ ("alpha", "α")
  , ("beta", "β")
  , ("gamma", "γ")
  , ("delta", "δ")
  -- ...
  , ("Gamma", "Γ")
  , ("Delta", "Δ")
  ]

bb :: Parser Text
bb = do
  exact "\\mathbb{"
  l <- asum (makeSymbolParser <$> bbs)
  exact "}"
  return l

bbs :: [(Text,Text)]
bbs =
  [ ("A", "𝔸")
  , ("B", "𝔹")
  , ("C", "ℂ")
  , ("N", "ℕ")
  , ("P", "ℙ")
  , ("Q", "ℚ")
  , ("R", "ℝ")
  , ("Z", "ℤ")
  ]

makeSymbolParser :: (Text, b) -> Parser b
makeSymbolParser (cmd, symb) = do
  exact cmd
  return symb