{-# LANGUAGE OverloadedLists #-}

module Base.Parser (module Base.Parser, module Export) where


import Language.Common (Var(..))
import Language.Expression (Expr(..), Prop(..))
import Language.Pattern (Patterns, Pattern, Shape(..), insertPattern, fromPatterns, makePattern)
import Parse.Token (TokStream, Tok(..), symbol, command)

import Control.Monad.Combinators.Expr as Export (Operator(..), makeExprParser)
import Control.Monad.State.Strict (State, get, put, modify)
import Text.Megaparsec as Export hiding (State, parse, sepBy1)

import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text


-- TODO: Replace `Void` with proper error component.
type Parser = ParsecT Void TokStream (State Registry)

data Registry = Registry
   { collectiveAdjs :: Patterns
   , distributiveAdjs :: Patterns
   , nominals :: Patterns
   , verbs :: Patterns
   , operators :: [[Operator Parser Expr]]
   , relators :: Map Tok (Expr -> Expr -> Prop)
   , varCount :: Word64
   }

initRegistry :: Registry
initRegistry = Registry
   { collectiveAdjs = primCollectiveAdjs
   , distributiveAdjs = mempty
   , nominals = primNominals
   , verbs = primVerbs
   , operators = primOperators
   , relators = primRelators
   , varCount = 0
   }
   where
   primOperators :: [[Operator Parser Expr]]
   primOperators =
      [ [ InfixR (makePrimOp (command "cdot") "prim_mul")
      ]
      , [ InfixR (makePrimOp (symbol "+") "prim_plus")
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
      , (Command "leq", \x y -> Predicate "prim_leq" `PredApp` x `PredApp` y)
      , (Command "geq", \x y -> Predicate "prim_geq" `PredApp` x `PredApp` y)
      , (Command "mid", \x y -> Predicate "prim_divides" `PredApp` x `PredApp` y)
      ]

   makeOp :: forall op a. Parser op -> a -> Parser a
   makeOp op constr = op *> pure constr
   {-# INLINE makeOp #-}

   primCollectiveAdjs :: Patterns
   primCollectiveAdjs = fromPatterns
      [ makePattern ["even"]
      , makePattern ["odd"]
      ]

   primNominals :: Patterns
   primNominals = fromPatterns
      [ makePattern ["natural", "number"]
      , makePattern ["rational", "number"]
      ]

   primVerbs :: Patterns
   primVerbs = fromPatterns
      [ makePattern ["divides", Slot]
      ]

makePrimOp :: forall op. Parser op -> Text -> Parser (Expr -> Expr -> Expr)
makePrimOp op prim = op *> pure (\x y -> Const prim `App` x `App` y)

getOperators :: Parser [[Operator Parser Expr]]
getOperators = operators <$> get
{-# INLINE getOperators #-}

-- TODO: Also handle priority and associativity.
registerOperator :: forall op. Parser op -> Text -> Parser ()
registerOperator op prim = do
   st <- get
   let ops = operators st
   let ops' = ops <> [[InfixR (makePrimOp op prim)]]
   put st{operators = ops'}

getRelators :: Parser (Map Tok (Expr -> Expr -> Prop))
getRelators = relators <$> get
{-# INLINE getRelators #-}

registerRelator :: Text -> Parser ()
registerRelator rel = do
   st <- get
   let rels = relators st
   let rels' = Map.insert (Command rel) (\x y -> Predicate rel `PredApp` x `PredApp` y) rels
   put st{relators = rels'}

getAdjs :: Parser Patterns
getAdjs = do
   st <- get
   -- TODO: biased merge potentially incorrect.
   pure (collectiveAdjs st <> distributiveAdjs st)
{-# INLINE getAdjs #-}

registerAdj :: Pattern -> Parser ()
registerAdj adj = do
   st <- get
   let adjs = collectiveAdjs st
   let adjs' = insertPattern adj adjs
   put st{collectiveAdjs = adjs'}

getNominals :: Parser Patterns
getNominals = nominals <$> get
{-# INLINE getNominals #-}

registerNominal :: Pattern -> Parser ()
registerNominal pat = do
   st <- get
   let pats = nominals st
   put st{nominals = insertPattern pat pats}

getVerbs :: Parser Patterns
getVerbs = verbs <$> get
{-# INLINE getVerbs #-}

registerVerb :: Pattern -> Parser ()
registerVerb pat = do
   st <- get
   let pats = verbs st
   put st{verbs = insertPattern pat pats}

getFreshVar :: Parser Var
getFreshVar = do
   regis <- get
   let k = varCount regis
   modify \st -> st{varCount = succ k}
   pure (Var ("x_" <> Text.pack (show k)))
   -- let regis' = regis{varCount = succ varCount}
   -- put regis'

noop :: (Applicative f) => f ()
noop = pure ()

many1 :: forall a. Parser a -> Parser (NonEmpty a)
many1 = NonEmpty.some
{-# INLINE many1 #-}

many1Till :: forall a end. Parser a -> Parser end -> Parser (NonEmpty a)
many1Till = NonEmpty.someTill
{-# INLINE many1Till #-}

-- | Parser negation. @never p@ succeeds iff when @p@ fails.
-- Consumes nothing and does not change any parser state.
never :: forall a. Parser a -> Parser ()
-- The name @notFollowedBy@ is a bit unintuitive (seems like a binary combinator).
never = notFollowedBy
{-# INLINE never #-}

endedBy :: forall a end. Parser a -> Parser end -> Parser a
p `endedBy` end = do
   result <- p
   end
   pure result
{-# INLINE endedBy #-}

-- | @sepEndedBy1 p sep@ parses one or more occurrences
-- of @p@, separated by @sep@ and mandatorily ended by @sep@.
-- Returns a nonempty list of the results of @p@.
sepEndedBy1 :: forall a sep. Parser a -> Parser sep -> Parser (NonEmpty a)
sepEndedBy1 = NonEmpty.endBy1
{-# INLINE sepEndedBy1 #-}

sepBy1 :: (MonadPlus m) => m a -> m sep -> m (NonEmpty a)
sepBy1 = NonEmpty.sepBy1


-- | Backtracking version of sepBy.
trySepBy :: (MonadParsec e s f) => f a -> f sep -> f [a]
trySepBy p sep = trySepBy1' p sep <|> pure []
{-# INLINE trySepBy #-}

-- | Backtracking version of sepBy1.
trySepBy1 :: (MonadParsec e s f) => f a -> f sep -> f (NonEmpty a)
trySepBy1 p sep = NonEmpty.fromList <$> trySepBy1' p sep
{-# INLINE trySepBy1 #-}

trySepBy1' :: (MonadParsec e s f) => f a -> f sep -> f [a]
trySepBy1' p sep = liftA2 (:) p (many (try (sep *> p)))
{-# INLINE trySepBy1' #-}
