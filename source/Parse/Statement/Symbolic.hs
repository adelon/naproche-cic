module Parse.Statement.Symbolic where


import Base.Parser
import Language.Expression (Expr, Prop)
import Parse.Expression (expression)
import Parse.Token (Tok, printTok, exactly, math)

import Data.Text (unpack)
import Data.Foldable (asum)

import qualified Data.Map.Strict as Map


data SymbolicStatement
  = RelatorChain RelatorChain
  deriving (Show, Eq, Ord)

symbolicStatement :: Parser SymbolicStatement
symbolicStatement = label "symbolic statement" $ math $ RelatorChain <$> relatorChain

-- It is convenient to have the predicate corresponding to a relator
-- saved in the data structure. But this comes at the cost of having to
-- define the `Show`, `Eq`, and `Ord` instances.

data RelatorChain
  = ChainBegin Expr
  | ChainRelator RelatorChain Tok (Expr -> Expr -> Prop) Expr

instance Show RelatorChain where
  show = \case
    ChainBegin e -> show e
    ChainRelator ch rel _ e -> show ch <> " " <> show rel <> " " <> show e

instance Eq RelatorChain where
  ch1 == ch2 = case compare ch1 ch2 of
    EQ -> True
    _ -> False

instance Ord RelatorChain where
  ChainBegin e1 `compare` ChainBegin e2 = e1 `compare` e2
  _ch `compare` ChainBegin _ = GT
  ChainBegin _ `compare` _ch = LT
  ChainRelator ch1 rel1 _ e1 `compare` ChainRelator ch2 rel2 _ e2 =
    case e1 `compare` e2 of
      EQ -> case rel1 `compare` rel2 of
        EQ -> ch1 `compare` ch2
        comp -> comp
      comp -> comp

relatorChain :: Parser RelatorChain
relatorChain = do
  e <- expression
  ch <- many1 chain
  return (makeChain e ch)
  where
    chain :: Parser (Expr, Tok, (Expr -> Expr -> Prop))
    chain = do
      (rel, predicate) <- relator
      e <- expression
      return (e, rel, predicate)
    makeChain e = \case
      [] -> ChainBegin e
      ((e', rel, predicate) : ch') -> ChainRelator (makeChain e ch') rel predicate e'


relator :: Parser (Tok, Expr -> Expr -> Prop)
relator = do
  rels <- getRelators
  let relToks = Map.keys rels
  rel <- asum (exactly <$> relToks)
  case Map.lookup rel rels of
    Just predicate -> return (rel, predicate)
    Nothing -> fail $ unpack $ "unknown relator" <> printTok rel
