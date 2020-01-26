module Parse.Statement where


import Base.Parser
import Language.Expression
import Language.Quantifier
import Parse.Expression (expression)
import Parse.Pattern (Pattern, patternWith)
import Parse.Token
import Parse.Var (Var(..), var, varList)

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
   negated = do
      try (word "it" *> word "is" *> word "not" *> word "the" *> word "case" *> word "that")
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
quantifierChain = label "quantification"
   (universal <|> almostUniversal <|> nonexistential <|> existential)
   where
   universal, almostUniversal, existential, nonexistential :: Parser (Prop -> Prop)
   universal = do
      word "all" <|> try (word "for" *> (word "every" <|> word "all"))
      (quantify, vs) <- varInfo Implies
      optional weHave
      pure (makeQuantification quantify Universal vs)
   almostUniversal = do
      try (word "almost" *> word "all") <|> try (word "for" *> word "almost" *> word "every" <|> word "all")
      (quantify, vs) <- varInfo Implies
      optional weHave
      pure (makeQuantification quantify Universal vs)
   nonexistential = do
      try (thereExists *> word "no")
      (quantify, vs) <- varInfo And
      optional suchThat
      pure (makeQuantification quantify Nonexistential vs)
   existential = do
      thereExists
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
   (word "for" *> (universal <|> existential)) <|> pure id
   where
   universal, existential :: Parser (Prop -> Prop)
   universal = do
      word "all" <|> word "every"
      (quantify, vs) <- varInfo Implies
      pure (makeQuantification quantify Universal vs)
   existential = do
      word "some"
      (quantify, vs) <- varInfo And
      pure (makeQuantification quantify Existential vs)

-- The operator `op` is used for the case of a quantification bounded by a proposition.
-- We replace this operator with `Implies` for the universal cases and `And` for the
-- existential cases.
varInfo :: (Prop -> Prop -> Prop) -> Parser (Prop -> Prop, NonEmpty (Typing Var Typ))
varInfo op = nominalInfo <|> symbolicInfo
   where
   nominalInfo = do
      (pat, info) <- nominal
      let quantifies = fst <$> info
      let args = snd <$> info
      let ty = (foldl App (ConstPattern pat) args)
      vs <- math varList
      pure (compose quantifies, (`Inhabits` ty) <$> vs)
   symbolicInfo = math do
      vs@(v0 :| _) <- var `sepBy1` comma
      related <- optional relator
      case related of
         Just rel -> do
            vs' <- var `sepBy1` comma
            let trafo p = makeChain (Free <$> toList vs) [(rel, Free <$> toList vs')] `op` p
            pure (trafo, (\v -> v `Inhabits` Hole) <$> vs)
         Nothing -> do
            ty <- ((symbol ":" <|> command "in") *> expression) <|> lookupVar v0
            pure (id, (`Inhabits` ty) <$> vs)

-- TODO: Implement proper precedence parsing.
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
atomicStatement = predication <|> contradiction <|> symbolicStatement
   where
   contradiction :: Parser Prop
   contradiction = Falsum <$ try (optional (word "a") *> word "contradiction")

   predication :: Parser Prop
   predication = do
      (quantify, arg) <- try term
      peeking <- optional continue
      case peeking of
         Just (Word "is") -> do
            (pat, info) <- adjective
            pure (makePredication pat arg quantify info)
         _otherwise -> do
            (pat, info) <- verb
            pure (makePredication pat arg quantify info)
      where
         continue :: Parser Tok
         continue = word "is"

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

symbolicStatement :: Parser Prop
symbolicStatement = label "symbolic statement" $ math relatorChain

relatorChain :: Parser Prop
relatorChain = do
   expr <- toList <$> expression `sepBy1` comma
   ch <- many chain
   pure (makeChain (expr) ch)
   where
      chain :: Parser (Relator, [Expr])
      chain = do
         rel <- relator
         expr <- toList <$> expression `sepBy1` comma
         pure (rel, expr)

makeChain :: [Expr] -> [(Relator, [Expr])] -> Prop
makeChain lhs body =
   let combos = [ Rel rel `PredApp` e1 `PredApp` e2 | e1 <- lhs, (rel, rhs) <- body, e2 <- rhs ]
   in  foldr1 And combos

type Relator = Tok

relator :: Parser Relator
relator = do
   rels <- getRelators
   let relToks = Map.keys rels
   asum (exactly <$> relToks)
