module Parse.Statement.Symbolic where


import Base.Parser
import Language.Expression (Expr)
import Parse.Expression (expression)
import Parse.Token (Tok, exactly, math)

import qualified Data.Map.Strict as Map


data SymbolicStatement
  = RelatorChain RelatorChain
  deriving (Show, Eq, Ord)

symbolicStatement :: Parser SymbolicStatement
symbolicStatement = label "symbolic statement" $ math $ RelatorChain <$> relatorChain

type Relator = Tok

data RelatorChain
  = ChainBegin Expr
  | ChainRelator RelatorChain Relator Expr
  deriving (Show, Eq, Ord)

relatorChain :: Parser RelatorChain
relatorChain = trace "relator chain" do
  expr <- expression
  ch <- many1 chain
  return (makeChain expr ch)
  where
    chain :: Parser (Relator, Expr)
    chain = do
      rel <- relator
      expr <- expression
      return (rel, expr)

    makeChain :: Expr -> [(Relator, Expr)] -> RelatorChain
    makeChain e = \case
      [] -> ChainBegin e
      ((rel, e') : ch') -> ChainRelator (makeChain e ch') rel e'

relator :: Parser Relator
relator = do
  rels <- getRelators
  let relToks = Map.keys rels
  asum (exactly <$> relToks)
