-- Basic parser data types and helper functions.

module Base.Parser (module Base.Parser, module Export) where


import Language.Expression (Expr(..), Prop(..))
import Language.Common (Var)

import Control.Monad.Combinators.Expr as Export
import Control.Monad.State.Strict
import Data.Foldable (asum)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text as Export (Text, pack)
import Data.Void
import Numeric.Natural (Natural)
import Text.Megaparsec as Export hiding (State)

import qualified Data.Map.Strict as Map
--import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as Lex


-- TODO: Replace `Void` with proper error component.
type Parser = ParsecT Void Text (State Registry)

-- Windows 3.1 sends kind regards!
data Registry = Registry
  { collectiveAdjs :: Set Text
  , distributiveAdjs :: Set Text
  , symbolicNotions :: Map Var Expr
  , operators :: [[Operator Parser Expr]]
  , idCount :: Natural
  }

initRegistry :: Registry
initRegistry = Registry
  { collectiveAdjs = mempty
  , distributiveAdjs = mempty
  , symbolicNotions = primSymbolicNotions
  , operators = primOperators
  , idCount = 0
  }
  where

    primSymbolicNotions :: Map Var Expr
    primSymbolicNotions = Map.fromList
      [ ("ℕ", Const "nat")
      ]

    primOperators :: [[Operator Parser Expr]]
    primOperators =
      [ [ InfixR (makePrimOp "+" "prim_plus")
        ]
      , [ InfixN (makeOp "=" \x y -> Prop (x `Equals` y))
        ]
      , [ InfixR (makeOp "\\times" (Times))
        , InfixR (makeOp "\\sqcup" (Plus))
        ]
      , [ InfixR (makeOp "\\land" \x y -> Prop (x `And` y))
        , InfixR (makeOp "\\lor"  \x y -> Prop (x `Or`  y))
        ]
      ]

    makeOp :: Text -> a -> Parser a
    makeOp op constr = do
      exact op
      return constr


makePrimOp :: Text -> Text -> Parser (Expr -> Expr -> Expr)
makePrimOp op prim = do
  exact op
  return (\x y -> Const prim `App` x `App` y)

getOperators :: MonadState Registry m => m [[Operator Parser Expr]]
getOperators = operators <$> get

-- TODO: Also handle priority and associativity.
registerOperator :: MonadState Registry m => Text -> Text -> m ()
registerOperator op prim = do
  st <- get
  let ops = operators st
  let ops' = ops <> [[InfixR (makePrimOp op prim)]]
  put st{operators = ops'}





many1 :: Parser a -> Parser [a]
many1 = some





-- `word` parses a word, ignoring case, and consume trailing whitespace.
word :: Text -> Parser Text
word w = do
  w' <- Lex.string' w
  Lex.space
  return w'

-- | Parses a specified literal and consumes trailing whitespace.
exact :: Text -> Parser Text
exact s = do
  Lex.string s
  Lex.space
  return s

period :: Parser ()
period = void (exact ".")

letter :: Parser Text
letter = do
  l <- Lex.letterChar
  Lex.space
  return (Text.singleton l)

iden :: Parser Expr
iden = Const <$> do
  Lex.string "\\iden{"
  name <- identifier
  Lex.char '}'
  Lex.space
  return name

constant :: Parser Expr
constant = Const <$> identifier

identifier :: Parser Text
identifier = pack <$> many1 Lex.letterChar





environment :: Text -> Parser a -> Parser a
environment env p = do
  exact ("\\begin{" <> env <> "}")
  Lex.space
  content <- p
  exact ("\\end{" <> env <> "}")
  Lex.space
  return content

environments :: [Text] -> Parser a -> Parser a
environments envs p = do
  Lex.string "\\begin{"
  env <- asum (Lex.string <$> envs)
  Lex.char '}'
  Lex.space
  content <- p
  Lex.string "\\end{"
  Lex.string env
  Lex.char '}'
  Lex.space
  return content

surroundedBy :: Text -> Text -> Parser a -> Parser a
surroundedBy open close p = do
  word open
  content <- p
  word close
  return content

-- | Turns a parser into a parser that parses the same thing,
-- but requires it to be embedded in a math environment.
math :: Parser a -> Parser a
math p = try (surroundedBy "$" "$" p)
  <|> try (surroundedBy "\\[" "\\]" p)
  <|> (surroundedBy "\\(" "\\)" p)

braced :: Parser a -> Parser a
braced = surroundedBy "{" "}"

parenthesized :: Parser a -> Parser a
parenthesized = surroundedBy "(" ")"

bracketed :: Parser a -> Parser a
bracketed = surroundedBy "[" "]"