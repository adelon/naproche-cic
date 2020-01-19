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

varInfo :: Parser (Var, Maybe Expr)
varInfo = do
  v <- var
  ty <- optional $ (command "in" <|> symbol ":") *> expression
  return (v, ty)

typing :: Parser (NonEmpty (Typing Var Typ))
typing = do
  vs <- var `sepBy1` comma
  ty <- (symbol ":" <|> command "in" >> expression) <|> return Hole
  return ((`Inhabits` ty) <$> vs)

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
  vs <- braced typing
  e <- expression
  return (foldr (\(v `Inhabits` ty) ex -> Pi v ty ex) e vs)
  -- return (Pi v ty e)
  -- return (foldr1 (\vs (v `Inhabits` ty) ->) )
  --foldPi :: Expr -> (NonEmpty (Typing Var Typ)) -> Expr
  --foldPi e vs = foldr (\(v `Inhabits` ty) ex -> Pi v ty ex) e vs

number :: Parser Expr
number = label "number" do
  n <- anyNumber
  return (Const n)
{-# INLINE number #-}
