module Parse.Prop where


import Base.Parser (Parser, getRelators, many1)
import Parse.Token (printTok, exactly)
import Parse.Expression (expression)
import Language.Expression (Expr, Prop)

import Data.Text (unpack)
import Data.Foldable (asum)

import qualified Data.Map.Strict as Map


data RelatorChain
  = ChainBegin Expr
  | ChainRelator RelatorChain (Expr -> Expr -> Prop) Expr

prop :: Parser RelatorChain
prop = do
  e <- expression
  ch <- many1 chain
  return (makeChain e ch)
  where
    chain :: Parser (Expr, (Expr -> Expr -> Prop))
    chain = do
      rel <- relator
      e <- expression
      return (e, rel)
    makeChain e = \case
      [] -> ChainBegin e
      ((e', rel) : ch') -> ChainRelator (makeChain e ch') rel e'


relator :: Parser (Expr -> Expr -> Prop)
relator = do
  relators <- getRelators
  let rels = Map.keys relators
  rel <- asum (exactly <$> rels)
  case Map.lookup rel relators of
    Just rel' -> return rel'
    Nothing -> fail $ unpack $ "unknown relator" <> printTok rel