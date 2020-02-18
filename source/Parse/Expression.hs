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
   pure (e `Inhabits` ty)

varInfo :: Parser (Var, Maybe Expr)
varInfo = do
   v <- var
   ty <- optional $ (command "in" <|> symbol ":") *> expression
   pure (v, ty)

typing :: Parser (NonEmpty (Typing Var Typ))
typing = do
   vs <- var `sepBy1` comma
   ty <- ((symbol ":" <|> command "in") *> expression) <|> pure Hole
   pure ((`Inhabits` ty) <$> vs)

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
   command "Pi"
   symbol "_"
   vs <- braced typing
   e <- expression
   pure (foldr (\(v `Inhabits` ty) ex -> Pi v ty ex) e vs)
   -- pure (Pi v ty e)
   -- pure (foldr1 (\vs (v `Inhabits` ty) ->) )
   --foldPi :: Expr -> (NonEmpty (Typing Var Typ)) -> Expr
   --foldPi e vs = foldr (\(v `Inhabits` ty) ex -> Pi v ty ex) e vs

number :: Parser Expr
number = label "number" do
   n <- anyNumber
   pure (Const n)
{-# INLINE number #-}
