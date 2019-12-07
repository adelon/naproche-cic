module Parse.Expression where

import Base.Parser
import Language.Expression
import Parse.Token
import Language.Common (Var(..))

import Prelude hiding (pi)


annotated :: Parser (Typing Expr Typ)
annotated = do
  e <- expression
  symbol ":" <|> command "in"
  ty <- expression
  return (e `Inhabits` ty)

typing :: Parser (Typing Var Typ)
typing = do
  v <- var
  symbol ":" <|> command "in"
  ty <- expression
  return (v `Inhabits` ty)

expression :: Parser Expr
expression = label "expression" do
  ops <- getOperators
  makeExprParser term ops

term :: Parser Expr
term = parenthesized expression
  <|> (Free <$> var)
  <|> pi

pi :: Parser Expr
pi = do
  try (command "Pi")
  symbol "_"
  v `Inhabits` ty <- braced typing
  e <- expression
  return (Pi v ty e)

var :: Parser Var
var = Var <$> variable
{-# INLINE var #-}