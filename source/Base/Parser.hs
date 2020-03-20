{-# LANGUAGE OverloadedLists #-}

module Base.Parser (module Base.Parser, module Export) where

import Base
import Base.Prims
import Language.Common (Var(..))
import Language.Expression (Expr(..), Typ)
import Language.Pattern
import Parse.Token (TokStream, Tok(..), symbol, command)

import Control.Monad.Combinators.Expr as Export (Operator(..), makeExprParser)
import Control.Monad.State.Strict (State, get, gets, put, modify)
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
   , funs :: Map Tok Text

   , relators :: Map Tok Text

   -- Counter used for generating fresh variables.
   , varCount :: Word64

   -- `proVar` tracks what the pronoun 'it' refers to.
   --
   , proVar :: Maybe Var
   , proCounter :: Word64

   --
   , fixedVars :: Map Var Typ
   }

initRegistry :: Registry
initRegistry = Registry
   { collectiveAdjs = mempty
   , distributiveAdjs = primDistributiveAdjs
   , nominals = primNominals
   , verbs = primVerbs
   , operators = primOperators
   , funs = primFuns
   , relators = primRelators
   , varCount = 0
   , proVar = Nothing
   , proCounter = 0
   , fixedVars = mempty
   }
   where
      primOperators :: [[Operator Parser Expr]]
      primOperators =
         [  [ InfixR (makePrimOp (command "mul") "prim_mul")
            ]
            , [ InfixR (makePrimOp (symbol "+") "prim_add")
            ]
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

getRelators :: Parser (Map Tok Text)
getRelators = relators <$> get
{-# INLINE getRelators #-}

registerRelator :: Text -> Parser ()
registerRelator rel = do
   st <- get
   let rels = relators st
   let rels' = Map.insert (Command rel) rel rels
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
   let adjs' = insertPattern adj (makeInterpretation adj) adjs
   put st{collectiveAdjs = adjs'}

getNominals :: Parser Patterns
getNominals = nominals <$> get
{-# INLINE getNominals #-}

registerNominal :: Pattern -> Parser ()
registerNominal pat = do
   st <- get
   let pats = nominals st
   put st{nominals = insertPattern pat (makeInterpretation pat) pats}

getVerbs :: Parser Patterns
getVerbs = verbs <$> get
{-# INLINE getVerbs #-}

registerVerb :: Pattern -> Parser ()
registerVerb pat = do
   st <- get
   let pats = verbs st
   put st{verbs = insertPattern pat (makeInterpretation pat) pats}

getFreshVar :: Parser Var
getFreshVar = do
   regis <- get
   let k = varCount regis
   modify \st -> st{varCount = succ k}
   pure (Var ("x_" <> Text.pack (show k)))
   -- let regis' = regis{varCount = succ varCount}
   -- put regis'

incrVar :: Registry -> Registry
incrVar regis@Registry{varCount = k} = regis{varCount = succ k}

fixVar :: Var -> Typ -> Parser ()
fixVar v ty = do
   info <- gets fixedVars
   let info' = Map.insert v ty info
   modify \st -> st{fixedVars = info'}

-- | See if a variable has been fixed beforehand. Returns the type
-- assigned to that variable if the variable has been fixed
-- and 'Hole' otherwise.
lookupVar :: Var -> Parser Typ
lookupVar v = gets (Map.findWithDefault Hole v . fixedVars)

lookupVars :: NonEmpty Var -> Parser (NonEmpty Typ)
lookupVars vs = traverse lookupVar vs

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

-- Helper function for the definition that uses NonEmpty.
trySepBy1' :: (MonadParsec e s f) => f a -> f sep -> f [a]
trySepBy1' p sep = liftA2 (:) p (many (try (sep *> p)))
{-# INLINE trySepBy1' #-}
