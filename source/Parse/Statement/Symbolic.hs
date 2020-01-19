module Parse.Statement.Symbolic where


import Base.Parser
import Language.Expression (Expr)
import Parse.Expression (expression)
import Parse.Token (Tok, exactly, math, comma)

import qualified Data.Map.Strict as Map


data SymbolicStatement
  = RelatorChain RelatorChain
  deriving (Show, Eq, Ord)

symbolicStatement :: Parser SymbolicStatement
symbolicStatement = label "symbolic statement" $ math $ RelatorChain <$> relatorChain

type Relator = Tok

data RelatorChain
  = ChainBegin (NonEmpty Expr)
  | ChainRelator RelatorChain Relator (NonEmpty Expr)
  deriving (Show, Eq, Ord)

relatorChain :: Parser RelatorChain
relatorChain = do
  expr <- expression `sepBy1` comma
  ch <- many chain
  return (makeChain expr ch)
  where
    chain :: Parser (Relator, NonEmpty Expr)
    chain = do
      rel <- relator
      expr <- expression `sepBy1` comma
      return (rel, expr)

    makeChain :: NonEmpty Expr -> [(Relator, (NonEmpty Expr))] -> RelatorChain
    makeChain e = \case
      [] -> ChainBegin e
      ((rel, e') : ch') -> ChainRelator (makeChain e ch') rel e'

relator :: Parser Relator
relator = do
  rels <- getRelators
  let relToks = Map.keys rels
  asum (exactly <$> relToks)
