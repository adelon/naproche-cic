{-# LANGUAGE OverloadedLists #-}

module Base.Parser (module Base.Parser, module Export) where


import Language.Expression (Expr(..), Prop(..))
import Language.Pattern (Patterns, PatternTree(..), Pattern, insertPattern)
import Parse.Token (TokStream, Tok(..), symbol, command)

import Control.Monad.Combinators.Expr as Export (Operator(..), makeExprParser)
import Control.Monad.State.Strict (State, get, put)
import Text.Megaparsec as Export hiding (State, parse, sepBy1)

import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set1 as Set1


-- TODO: Replace `Void` with proper error component.
type Parser = ParsecT Void TokStream (State Registry)

-- Windows 3.1 sends kind regards!
data Registry = Registry
  { collectiveAdjs :: Set Text
  , distributiveAdjs :: Set Text
  , nominals :: Patterns
  , operators :: [[Operator Parser Expr]]
  , relators :: Map Tok (Expr -> Expr -> Prop)
  , idCount :: Natural
  }

initRegistry :: Registry
initRegistry = Registry
  { collectiveAdjs = primCollectiveAdjs
  , distributiveAdjs = mempty
  , nominals = primNominals
  , operators = primOperators
  , relators = primRelators
  , idCount = 0
  }
  where
    primOperators :: [[Operator Parser Expr]]
    primOperators =
      [ [ InfixR (makePrimOp (symbol "+") "prim_plus")
        ]
      , [ InfixR (makePrimOp (command "mid") "prim_divides")
        ]
      , [ InfixR (makeOp (command "times") (Times))
        , InfixR (makeOp (command "sqcup") (Plus))
        ]
      ]
    primRelators :: Map Tok (Expr -> Expr -> Prop)
    primRelators = Map.fromList
      [ (Symbol "=", \x y -> Predicate "prim_eq" `PredApp` x `PredApp` y)
      , (Command "neq", \x y -> Predicate "prim_not_eq" `PredApp` x `PredApp` y)
      , (Symbol "<", \x y -> Predicate "prim_less" `PredApp` x `PredApp` y)
      , (Command "leq", \x y -> Predicate "prim_less_eq" `PredApp` x `PredApp` y)
      ]

    makeOp :: Parser op -> a -> Parser a
    makeOp op constr = op >> return constr
    {-# INLINE makeOp #-}

    primCollectiveAdjs :: Set Text
    primCollectiveAdjs = Set.fromList ["even", "odd"]

    primNominals :: Patterns
    primNominals = Set1.fromNonEmpty
      [ PatternContinue "natural" $ Set1.fromNonEmpty
        [ PatternContinue "number" $ Set1.fromNonEmpty [PatternEnd]
        , PatternEnd
        ]
      ]

makePrimOp :: Parser op -> Text -> Parser (Expr -> Expr -> Expr)
makePrimOp op prim = op >> return (\x y -> Const prim `App` x `App` y)

getOperators :: Parser [[Operator Parser Expr]]
getOperators = operators <$> get
{-# INLINE getOperators #-}

getRelators :: Parser (Map Tok (Expr -> Expr -> Prop))
getRelators = relators <$> get
{-# INLINE getRelators #-}

getAdjs :: Parser (Set Text)
getAdjs = do
  st <- get
  return (collectiveAdjs st `Set.union` distributiveAdjs st)
{-# INLINE getAdjs #-}

getNominals :: Parser Patterns
getNominals = nominals <$> get
{-# INLINE getNominals #-}

-- TODO: Also handle priority and associativity.
registerOperator :: Parser op -> Text -> Parser ()
registerOperator op prim = do
  st <- get
  let ops = operators st
  let ops' = ops <> [[InfixR (makePrimOp op prim)]]
  put st{operators = ops'}

registerNominal :: Pattern -> Parser ()
registerNominal pat = do
  st <- get
  let pats = nominals st
  put st{nominals = insertPattern pat pats}


noop :: Applicative f => f ()
noop = pure ()

many1 :: Parser a -> Parser [a]
many1 = some
{-# INLINE many1 #-}

many1Till :: Parser a -> Parser end -> Parser (NonEmpty a)
many1Till = NonEmpty.someTill
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

-- | @sepEndedBy1 p sep@ parses one or more occurrences
-- of @p@, separated by @sep@ and mandatorily ended by @sep@.
-- Returns a nonempty list of the results of @p@.
sepEndedBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepEndedBy1 = NonEmpty.endBy1
{-# INLINE sepEndedBy1 #-}

sepBy1 :: MonadPlus m => m a -> m sep -> m (NonEmpty a)
sepBy1 = NonEmpty.sepBy1


-- | Backtracking version of sepBy.
trySepBy :: MonadParsec e s f => f a -> f sep -> f [a]
trySepBy p sep = trySepBy1' p sep <|> pure []
{-# INLINE trySepBy #-}

-- | Backtracking version of sepBy1.
trySepBy1 :: MonadParsec e s f => f a -> f sep -> f (NonEmpty a)
trySepBy1 p sep = NonEmpty.fromList <$> trySepBy1' p sep
{-# INLINE trySepBy1 #-}

trySepBy1' :: MonadParsec e s f => f a -> f sep -> f [a]
trySepBy1' p sep = liftA2 (:) p (many (try (sep *> p)))
{-# INLINE trySepBy1' #-}
