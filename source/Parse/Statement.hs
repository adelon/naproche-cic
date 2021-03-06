module Parse.Statement where


import Base
import Base.Parser
import Language.Expression
import Language.Quantifier
import Parse.Expression (expression)
import Parse.Pattern (Pattern, patternWith)
import Parse.Token
import Parse.Var (var, varList)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map


type Statement = Prop

statement :: Parser Statement
statement = headedStatement <|> (unheadedStatement <**> lateQuantification)


headedStatement :: Parser Prop
headedStatement = quantified <|> ifThen <|> negated
   where
   quantified :: Parser Prop
   quantified = do
      quantify <- quantifierChain
      optional comma
      optional weHave
      stmt <- statement
      pure (quantify stmt)

   negated :: Parser Prop
   negated = label "negated statement" do
      try (words ["it", "is", "not", "the", "case", "that"])
      Not <$> statement

   ifThen :: Parser Prop
   ifThen = do
      word "if"
      stmt1 <- statement
      optional comma
      word "then"
      stmt2 <- statement
      pure (stmt1 `Implies` stmt2)


-- Parses a quantification returning a quantification function.
--
quantifierChain :: Parser (Prop -> Prop)
quantifierChain = label "quantification" (universal <|> existential)
   where
   universal, existential :: Parser (Prop -> Prop)
   universal = do
--
--    We require the use of the word 'for' before the word 'every'
--    in quantifications, to distinguish it from the use of
--    'every' in quantifier terms. Consider the following examples.
--
--    'for every natural number n we have that n <predication>' (quantification)
--    'every natural number <predication>'                     (quantified term)
--
--    vvvvvvvvvv
      word "for"
      almost <- optional (word "almost")
      word "every" <|> word "all"
      (quantify, vs) <- varInfo Implies
      suchThatCond <- optional do
         suchThat
         stmt <- atomicStatement
         comma
         return stmt
      let
         quantify' = case suchThatCond of
            Nothing -> quantify
            Just stmt -> (stmt `Implies`) . quantify
      optional weHave
      case almost of
         Nothing -> pure (makeQuantification quantify' Universal vs)
         Just _  -> pure (makeQuantification quantify' AlmostUniversal vs)
   existential = do
      thereExists
      no <- optional (word "no")
      case no of
         Just _ -> do
            (quantify, vs) <- varInfo And
            optional suchThat
            pure (makeQuantification quantify Nonexistential vs)
         Nothing -> do
            optional (word "a")
            unique <- optional (word "unique")
            (quantify, vs) <- varInfo And
            optional suchThat
            pure case unique of
               Nothing -> makeQuantification quantify Existential vs
               Just _  -> makeQuantification quantify UniqueExistential vs

makeQuantification :: (Prop -> Prop) -> Quantifier -> NonEmpty (Typing Var Typ) -> (Prop -> Prop)
makeQuantification quantify quant vs = \p -> compose quantifications (quantify p)
   where
   quantifications = (\(v `Inhabits` ty) -> Quantify quant v ty) <$> vs


-- | Parses an optional late quantification.
-- Returns a quantifying function, defaulting to @id@ if there are
-- no late quantifications. This parser would usually be used with
-- the applicative combinator @(<**>)@, which applies the quantifying
-- function to the proceeding parser.
lateQuantification :: Parser (Prop -> Prop)
lateQuantification = label "late quantification"
   (word "for" *> (universal <|> existential <|> nonexistential)) <|> pure id
   where
   universal, existential, nonexistential :: Parser (Prop -> Prop)
   universal = do
      word "all" <|> word "every"
      (quantify, vs) <- varInfo Implies
      pure (makeQuantification quantify Universal vs)
   existential = do
      word "some"
      unique <- optional (word "unique")
      (quantify, vs) <- varInfo And
      pure case unique of
         Nothing -> makeQuantification quantify Existential vs
         Just _  -> makeQuantification quantify UniqueExistential vs
   nonexistential = do
      word "no"
      (quantify, vs) <- varInfo And
      pure (makeQuantification quantify Nonexistential vs)

-- The operator `op` is used for the case of a quantification bounded by a proposition.
-- We replace this operator with `Implies` for the universal cases and `And` for the
-- existential cases.
varInfo :: (Prop -> Prop -> Prop) -> Parser (Prop -> Prop, NonEmpty (Typing Var Typ))
varInfo op = symbolicInfo <|> nominalInfo
   where
   nominalInfo = do
      (pat, info) <- nominal
      let quantifies = fst <$> info
      let args = snd <$> info
      let ty = (foldl App (ConstPattern pat) args)
      vs <- math varList
      pure (compose quantifies, (`Inhabits` ty) <$> vs)
   symbolicInfo = math do
      vs <- var `sepBy1` comma
      related <- optional relator
      case related of
         Just rel -> do
            vs' <- var `sepBy1` comma
            let trafo p = makeChain (Free <$> toList vs) [(rel, Free <$> toList vs')] `op` p
            pure (trafo, (\v -> v `Inhabits` Hole) <$> vs)
         Nothing -> do
            typs <- ((symbol ":" <|> command "in") *> (pure <$> expression)) <|> lookupVars vs
            pure (id, NonEmpty.zipWith Inhabits vs typs)


-- TODO: Implement ForTheL-like chain parsing.
--
unheadedStatement :: Parser Prop
unheadedStatement = do
   stmt1 <- atomicStatement
   peeking <- optional continue
   case peeking of
      Just (Word "and") -> do
         stmt2 <- statement
         pure (stmt1 `And` stmt2)
      Just (Word "or") -> do
         stmt2 <- statement
         pure (stmt1 `Or` stmt2)
      Just (Word "iff") -> do
         stmt2 <- statement
         pure (stmt1 `Or` stmt2)
      Just (Word "implies") -> do
         optional (word "that")
         stmt2 <- statement
         pure (stmt1 `Implies` stmt2)
      -- Just (Word "where") -> do
      --    info <- whereStatement
      --    pure (StatementWhere stmt1 info)
      _otherwise -> pure stmt1
   where
   continue :: Parser Tok
   continue = word "and" <|> word "or" <|> iff <|> word "where" <|> word "implies"

   -- TODO: Implement where statements.
   --
   -- The information could either be stored in the parse tree or added to some
   -- local environment.
   --
   -- whereStatement :: Parser WhereStatement
   -- whereStatement = sepByComma1 do
   --   begin "math"
   --   v <- var
   --   symbol "=" <|> (end "math" *> word "is" *> begin "math")
   --   expr <- expression
   --   end "math"
   --   pure (v, expr)

atomicStatement :: Parser Prop
atomicStatement = atomicStatementSymbolic <|> atomicStatementTextual

-- Atomic statements beginning with symbols.
--
atomicStatementSymbolic :: Parser Prop
atomicStatementSymbolic = do
   begin "math"
   exprs <- toList <$> expression `sepBy1` comma
   ended <- optional (end "math")
   case ended of
--
--    Here we continue with a textual statement.
--    vvvvvvvvvvvvvvvv
      Just _end -> predication id exprs
--
--    In this case the statement must be purely symbolic,
--    and hence a predication or a chain of relators.
--    vvvvvvv
      _notEnded -> do
         ch <- many chain
         end "math"
         pure (makeChain exprs ch)
   where
      chain :: Parser (Relator, [Expr])
      chain = label "relator chain" do
         rel <- relator
         exprs <- toList <$> expression `sepBy1` comma
         pure (rel, exprs)


makeChain :: [Expr] -> [(Relator, [Expr])] -> Prop
makeChain lhs body =
   let combos = [ Rel rel `PredApp` e1 `PredApp` e2 | e1 <- lhs, (rel, rhs) <- body, e2 <- rhs ]
   in  foldr1 And combos


-- Atomic statements beginning with words.
--
atomicStatementTextual :: Parser Prop
atomicStatementTextual = contradiction <|> do
   (quantify, arg) <- try term
   peeking <- optional (word "is")
   case peeking of
      Just (Word "is") -> do
         (pat, info) <- adjective
         pure (makePredication pat arg quantify info)
      _otherwise -> do
         (pat, info) <- verb
         pure (makePredication pat arg quantify info)

contradiction :: Parser Prop
contradiction = Falsum <$ try (optional (word "a") *> word "contradiction")

-- Predication, depending on the subjects `args` and pending quantifications `quantify`.
--
predication :: (Prop -> Prop) -> [Expr] -> Parser Prop
predication quantify args = do
   let num = length args
   peeking <- optional (copulaPlural num)
   case peeking of
--
--    The peeking token was a copula of matching grammatical number.
--    vvvvvv
      Just _ -> do
         (pat, info) <- adjective
         pure case args of
            [arg]  -> makePredication pat arg quantify info
            -- TODO: here we need to distinguish between collective and distributive adjectives.
            _multi -> error "plural predication not implemented yet!"
--
--    We continue with a (non-copula) verbal pattern.
--    vvvvv
      Nothing -> do
         -- TODO: require a verb of correct grammatical number.
         (pat, info) <- verb
         pure case args of
            [arg]  -> makePredication pat arg quantify info
            -- Desugar predications with regular verbs and multi-subjects.
            _multi -> error "plural predication not implemented yet!"


makePredication :: Pattern -> Expr -> (Prop -> Prop) -> [(Prop -> Prop, Expr)] -> Prop
makePredication pat arg quantify info =
   quantify (compose quantifies (patternPredication pat (arg : args)))
      where
      args = snd <$> info
      quantifies = fst <$> info


term :: Parser (Prop -> Prop, Expr)
term = quantifiedTerm <|> definiteTerm
   where
   definiteTerm = (\e -> (id, e)) <$> math expression

quantifiedTerm :: Parser (Prop -> Prop, Expr)
quantifiedTerm = label "quantified term"
   (universal <|> almostUniversal <|> existential <|> nonexistential)
   where
   universal, almostUniversal, existential, nonexistential :: Parser (Prop -> Prop, Expr)
   universal = word "every" *> indefiniteTerm Universal
   almostUniversal = try (word "almost" *> word "every") *> indefiniteTerm AlmostUniversal
   nonexistential = word "no" *> indefiniteTerm Nonexistential
   existential = word "some" *> do
      unique <- optional (word "unique")
      case unique of
         Nothing -> indefiniteTerm Existential
         Just _  -> indefiniteTerm UniqueExistential
   indefiniteTerm quant = do
      (pat, info) <- nominal
      let quantifies = fst <$> info
      let args = snd <$> info
      v <- getFreshVar
      let ty = (foldl App (ConstPattern pat) args)
      let quantify = Quantify quant v ty
      pure (quantify . compose quantifies, Free v)


type Nominal = (Pattern, [(Prop -> Prop, Expr)])
type Adj     = (Pattern, [(Prop -> Prop, Expr)])
type Verb    = (Pattern, [(Prop -> Prop, Expr)])

nominal :: Parser Nominal
nominal = do
   pats <- getNominals
   patternWith term pats
{-# INLINE nominal #-}

adjective :: Parser Adj
adjective = label "adjective" do
   pats <- getAdjs
   patternWith term pats
{-# INLINE adjective #-}

verb :: Parser Verb
verb = label "verb" do
   pats <- getVerbs
   patternWith term pats
{-# INLINE verb #-}


type Relator = Tok

relator :: Parser Relator
relator = do
   rels <- getRelators
   let relToks = Map.keys rels
   asum (exactly <$> relToks)
