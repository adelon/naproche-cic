module Parse.Expression where

import Base.Parser
import Language.Expression

import Parse.Var (var)



annotated :: Parser (Typing Expr Typ)
annotated = do
  e1 <- expr
  exact ":" <|> exact "\\in"
  e2 <- expr
  return (e1 `Inhabits` e2)

typing :: Parser (Typing Var Typ)
typing = do
  v <- var
  exact ":" <|> exact "\\in"
  ty <- expr
  return (v `Inhabits` ty)

expr :: Parser Expr
expr = label "expression" do
  ops <- getOperators
  makeExprParser term ops

term :: Parser Expr
term = parenthesized expr
  <|> try (Free <$> var)
  <|> try iden
  <|> constant
