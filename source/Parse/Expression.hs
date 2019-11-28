module Parse.Expression where

import Base.Parser
import Language.Expression

import Parse.Var (var)

import Prelude hiding (pi)


annotated :: Parser (Typing Expr Typ)
annotated = do
  e <- expression
  exact ":" <|> exact "\\in"
  ty <- expression
  return (e `Inhabits` ty)

typing :: Parser (Typing Var Typ)
typing = do
  v <- var
  exact ":" <|> exact "\\in"
  ty <- expression
  return (v `Inhabits` ty)

expression :: Parser Expr
expression = label "expression" do
  ops <- getOperators
  makeExprParser term ops

term :: Parser Expr
term = parenthesized expression
  <|> try (Free <$> var)
  <|> try iden
  <|> pi
  <|> constant

pi :: Parser Expr
pi = do
  try (command "Pi")
  exact "_"
  v `Inhabits` ty <- braced typing
  e <- expression
  return (Pi v ty e)
