module Parse.Statement where


import Base.Parser
import Language.Common (Var)
import Language.Expression (Expr(..), Typ, Typing(..))
import Language.Quantifier
import Parse.Expression (expression)
import Parse.Pattern (Pattern, patternWith)
import Parse.Statement.Symbolic (SymbolicStatement, symbolicStatement)
import Parse.Token (word, symbol, command, begin, end, math, comma, sepByComma1)
import Parse.Token (iff, thereExists, suchThat)
import Parse.Var (var)
import Tokenize (Tok(..))


data Statement
  = StatementHeaded HeadedStatement
  | StatementUnheaded UnheadedStatement
  -- | StatementChain Chain
  deriving (Show, Eq, Ord)

statement :: Parser Statement
statement = (StatementHeaded <$> headedStatement)
  <|> (StatementUnheaded <$> unheadedStatement)

data HeadedStatement
  = StatementQuantified (NonEmpty (Quantifier, Typing Var Typ)) Statement
  | StatementImplication Statement Statement
  | StatementNegated Statement
  deriving (Show, Eq, Ord)

headedStatement :: Parser HeadedStatement
headedStatement = quantified <|> ifThen <|> negated
  where
    quantified :: Parser HeadedStatement
    quantified = do
      info <- quantifierChain
      stmt <- statement
      return (StatementQuantified info stmt)

    negated :: Parser HeadedStatement
    negated = do
      try (word "it" *> word "is" *> word "not" *> word "the" *> word "case" *> word "that")
      StatementNegated <$> statement

    ifThen :: Parser HeadedStatement
    ifThen = do
      word "if"
      stmt1 <- statement
      optional comma
      word "then"
      stmt2 <- statement
      return (StatementImplication stmt1 stmt2)

quantifierChain :: Parser (NonEmpty (Quantifier, Typing Var Typ))
quantifierChain = do
  (quant, vs) <- quantifiedNominal
  let chain = (\v -> (quant, v)) <$> vs
  return chain

quantifiedNominal :: Parser (Quantifier, NonEmpty (Typing Var Typ))
quantifiedNominal = label "quantified nominal" (universal <|> existential <|> nonexistential)
  where
    universal, existential, nonexistential :: Parser (Quantifier, NonEmpty (Typing Var Typ))
    universal = do
      word "all" <|> try (word "for" >> word "every")
      varInfo <- typing
      -- TODO this needs to be registered as local variable information.
      optional (word "we" >> word "have" >> word "that")
      return (Universal, varInfo)
    existential = do
      thereExists
      varInfo <- typing
      optional suchThat
      -- TODO this needs to be registered as local variable information.
      return (Existential, varInfo)
    nonexistential = do
      word "no"
      varInfo <- typing
      -- TODO this needs to be registered as local variable information.
      return (Nonexistential, varInfo)
    typing :: Parser (NonEmpty (Typing Var Typ))
    typing = math do
      vs <- var `sepBy1` comma
      command "in"
      ty <- expression
      return ((`Inhabits` ty) <$> vs)

-- WARN/FIXME: the following results in no precedence between
-- 'and'/'or'. This may require fixing in processing.

data UnheadedStatement
  = StatementConjunction AtomicStatement Statement
  | StatementDisjunction AtomicStatement Statement
  | StatementIff AtomicStatement Statement
  | StatementImplies AtomicStatement Statement
  | StatementWhere AtomicStatement WhereStatement
  | StatementAtomic AtomicStatement
  deriving (Show, Eq, Ord)

unheadedStatement :: Parser UnheadedStatement
unheadedStatement = do
  stmt1 <- atomicStatement
  peeking <- optional continue
  case peeking of
    Just (Word "and") -> do
      stmt2 <- statement
      return (StatementConjunction stmt1 stmt2)
    Just (Word "or") -> do
      stmt2 <- statement
      return (StatementDisjunction stmt1 stmt2)
    Just (Word "iff") -> do
      stmt2 <- statement
      return (StatementIff stmt1 stmt2)
    Just (Word "implies") -> do
      stmt2 <- statement
      return (StatementImplies stmt1 stmt2)
    Just (Word "where") -> do
      info <- whereStatement
      return (StatementWhere stmt1 info)
    -- The above exhausts all cases where a token was consumed.
    -- Below is how we proceed when we cannot consume a continuation token.
    _otherwise -> do
      return (StatementAtomic stmt1)
  where
    continue :: Parser Tok
    continue = word "and" <|> word "or" <|> iff <|> word "where" <|> word "implies"

    whereStatement :: Parser WhereStatement
    whereStatement = sepByComma1 do
      begin "math"
      v <- var
      symbol "=" <|> (end "math" *> word "is" *> begin "math")
      expr <- expression
      end "math"
      return (v, expr)



-- Each entry of the nonempty list represents the assertion that
-- the variable is equal to the expression.
type WhereStatement = NonEmpty (Var, Expr)

data AtomicStatement
  = Thesis   -- ^ The current goal.
  | Contrary -- ^ Negation of the current goal.
  | Contradiction -- ^ Bottom.
  | SymbolicStatement SymbolicStatement
  | PredicativeAdj Term Adj
  | PredicativeVerb Term Verb
  deriving (Show, Eq, Ord)

atomicStatement :: Parser AtomicStatement
atomicStatement = predication <|> constStatement
  <|> (SymbolicStatement <$> symbolicStatement)
  where
    constStatement, thesis, contrary, contradiction :: Parser AtomicStatement
    constStatement = thesis <|> contrary <|> contradiction
    thesis = Thesis <$ try (optional (word "the") *> word "thesis")
    contrary = Contrary <$ try (optional (word "the") *> word "contrary")
    contradiction = Contradiction <$ try (optional (word "a") *> word "contradiction")

    predication :: Parser AtomicStatement
    predication = do
      n <- try term
      peeking <- optional continue
      case peeking of
        Just (Word "is") -> do
          adj <- adjective
          return (PredicativeAdj n adj)
        -- The above exhausts all cases where a token was consumed.
        -- Below is how we proceed when we cannot consume a continuation token.
        _otherwise ->
          PredicativeVerb n <$> verb
      where
        continue :: Parser Tok
        continue = word "is"

data Term
  = TermDefiniteSymbolic Expr
  | TermDefiniteNoun
  | TermQuantified Quantifier Expr
  deriving (Show, Eq, Ord)

term :: Parser Term
term = quantifiedTerm <|> (TermDefiniteSymbolic <$> math expression)

quantifiedTerm :: Parser Term
quantifiedTerm = label "quantified term" (universal <|> existential <|> nonexistential)
  where
    universal, existential, nonexistential :: Parser Term
    universal = do
      try (word "every")
      noun <- nominal
      return (TermQuantified Universal noun)
    existential = do
      try (word "some")
      noun <- nominal
      return (TermQuantified Existential noun)
    nonexistential = do
      try (word "no")
      noun <- nominal
      return (TermQuantified Nonexistential noun)

type Nominal = Expr

nominal :: Parser Expr
nominal = do
  pats <- getNominals
  (pat, es) <- patternWith (math expression) pats
  return (foldl App (ConstPattern pat) es)

type Adj = (Pattern, [Term])

adjective :: Parser Adj
adjective = label "adjective" do
  pats <- getAdjs
  adj <- patternWith term pats
  return adj

type Verb = (Pattern, [Term])

verb :: Parser Verb
verb = label "verb" do
  pats <- getVerbs
  vrb <- patternWith term pats
  return vrb
