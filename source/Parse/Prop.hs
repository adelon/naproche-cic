module Parse.Prop where


import Base.Parser (Parser, getRelators, many1)
import Parse.Token (Tok, exactly)
import Parse.Expression (expression)
import Language.Expression (Expr)

import Data.Foldable (asum)

import qualified Data.Map.Strict as Map


data RelatorChain
  = ChainBegin Expr
  | ChainRelator RelatorChain Tok Expr

prop :: Parser RelatorChain
prop = do
  e <- expression
  ch <- many1 chain
  return (makeChain e ch)
  where
    chain = error "Parse.Prop.prop/chain"
    makeChain e = \case
      [] -> ChainBegin e
      ((e', rel) : ch') -> ChainRelator (makeChain e ch') rel e'


relator :: Parser Tok
relator = do
  relators <- getRelators
  let rels = Map.keys relators
  asum (exactly <$> rels)