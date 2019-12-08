-- Basic parser data types and helper functions.

module Base.Parser (module Base.Parser, module Export) where


import Language.Expression (Expr(..), Prop(..))
import Parse.Token

import Control.Monad.Combinators.Expr as Export
import Control.Monad.State.Strict
import Data.Set (Set)
import Data.Map (Map)
import Data.Text as Export (Text, pack)
import Data.Void
import Numeric.Natural (Natural)
import Text.Megaparsec as Export hiding (State, parse)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- TODO: Replace `Void` with proper error component.
type Parser = ParsecT Void TokStream (State Registry)

-- Windows 3.1 sends kind regards!
data Registry = Registry
  { collectiveAdjs :: Set Text
  , distributiveAdjs :: Set Text
  , ofNotions :: Map Text Expr
  , operators :: [[Operator Parser Expr]]
  , idCount :: Natural
  }

initRegistry :: Registry
initRegistry = Registry
  { collectiveAdjs = mempty
  , distributiveAdjs = mempty
  , operators = primOperators
  , ofNotions = primOfNotions
  , idCount = 0
  }
  where
    primOperators :: [[Operator Parser Expr]]
    primOperators =
      [ [ InfixR (makePrimOp (symbol "+") "prim_plus")
        ]
      , [ InfixN (makeOp (symbol "=") \x y -> Prop (x `Equals` y))
        ]
      , [ InfixR (makeOp (command "times") (Times))
        , InfixR (makeOp (command "sqcup") (Plus))
        ]
      , [ InfixR (makeOp (command "land") \x y -> Prop (x `And` y))
        , InfixR (makeOp (command "lor")  \x y -> Prop (x `Or`  y))
        ]
      ]

    makeOp :: Parser op -> a -> Parser a
    makeOp op constr = op >> return constr
    {-# INLINE makeOp #-}

    primOfNotions :: Map Text Expr
    primOfNotions = Map.fromList [("successor", Const "succ")]

makePrimOp :: Parser op -> Text -> Parser (Expr -> Expr -> Expr)
makePrimOp op prim = op >> return (\x y -> Const prim `App` x `App` y)

getOperators :: Parser [[Operator Parser Expr]]
getOperators = operators <$> get
{-# INLINE getOperators #-}

-- TODO: Also handle priority and associativity.
registerOperator :: Parser op -> Text -> Parser ()
registerOperator op prim = do
  st <- get
  let ops = operators st
  let ops' = ops <> [[InfixR (makePrimOp op prim)]]
  put st{operators = ops'}

getAdjs :: Parser (Set Text)
getAdjs = do
  st <- get
  return (collectiveAdjs st `Set.union` distributiveAdjs st)
{-# INLINE getAdjs #-}

many1 :: Parser a -> Parser [a]
many1 = some
{-# INLINE many1 #-}

many1Till :: Parser a -> Parser end -> Parser [a]
many1Till = someTill
{-# INLINE many1Till #-}

-- | Parser negation. @never p@ succeeds iff when @p@ fails.
-- Consumes nothing and does not change any parser state.
never :: Parser a -> Parser ()
-- The name @notFollowedBy@ is a bit unintuitive (seems like a binary combinator).
never = notFollowedBy
{-# INLINE never #-}

endedBy :: Parser a -> Parser end -> Parser a
p `endedBy` end = do
  result <- p
  end
  return result
{-# INLINE endedBy #-}
