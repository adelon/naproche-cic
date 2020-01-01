module Parse.Expression
  ( module Parse.Expression
  , module Language.Expression
  ) where


import Base.Parser
import Language.Expression
import Parse.Token
import Parse.Var


annotated :: Parser (Typing Expr Typ)
annotated = do
  e <- expression
  symbol ":" <|> command "in"
  ty <- expression
  return (e `Inhabits` ty)

varInfo = do
  v <- var
  ty <- optional $ (command "in" <|> symbol ":") *> expression
  return (v, ty)

typing :: Parser (Typing Var Typ)
typing = do
  v <- var
  symbol ":" <|> command "in"
  ty <- expression
  return (v `Inhabits` ty)

expression :: Parser Expr
expression = label "expression" do
  ops <- getOperators
  makeExprParser expression' ops

expression' :: Parser Expr
expression' = parenthesized expression
  <|> (Free <$> var)
  <|> pi
  <|> number

pi :: Parser Expr
pi = do
  try (command "Pi")
  symbol "_"
  v `Inhabits` ty <- braced typing
  e <- expression
  return (Pi v ty e)

number :: Parser Expr
number = label "number" do
  n <- anyNumber
  return (Const n)
