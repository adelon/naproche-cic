module Parse.Statement where


import Base.Parser (Parser, label, try, (<|>), satisfy, optional)
import Base.Parser (getNominals, getAdjs)
import Language.Common (Var)
import Language.Expression (Expr(..), Typ, Typing(..))
import Language.Quantifier
import Parse.Expression (expression)
import Parse.Pattern (patternWith)
import Parse.Statement.Symbolic (SymbolicStatement, symbolicStatement)
import Parse.Token (math, word, command, comma, iff)
import Parse.Var (var)
import Tokenize (Tok(..), Located(..))

import qualified Data.Set as Set


type Adj = Text

data Statement
  = StatementHeaded HeadedStatement
  | StatementUnheaded UnheadedStatement
  -- | StatementChain Chain
  deriving (Show, Eq, Ord)

statement :: Parser Statement
statement = trace "parsing statement"
  (StatementHeaded <$> headedStatement)
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
  info <- quantifiedNotion
  return (pure info)

quantifiedNotion :: Parser (Quantifier, Typing Var Typ)
quantifiedNotion = label "quantified notion" (universal <|> existential <|> nonexistential)
  where
    universal, existential, nonexistential :: Parser (Quantifier, Typing Var Typ)
    universal = do
      word "all" <|> try (word "for" >> word "every")
      varInfo <- typing
      -- TODO this needs to be registered as local variable information.
      optional (word "we" >> word "have" >> word "that")
      return (Universal, varInfo)
    existential = do
      word "some"
      varInfo <- typing
      -- TODO this needs to be registered as local variable information.
      return (Existential, varInfo)
    nonexistential = do
      word "no"
      varInfo <- typing
      -- TODO this needs to be registered as local variable information.
      return (Nonexistential, varInfo)
    typing :: Parser (Typing Var Typ)
    typing = math do
      v <- var
      command "in"
      ty <- expression
      return (v `Inhabits` ty)

data UnheadedStatement
  = StatementConjunction AtomicStatement Statement
  | StatementIff AtomicStatement Statement
  | StatementAtomic AtomicStatement
  deriving (Show, Eq, Ord)

unheadedStatement :: Parser UnheadedStatement
unheadedStatement = do
  stmt1 <- atomicStatement
  c <- optional continue
  case c of
    Just (Word "and") -> do
      stmt2 <- statement
      return (StatementConjunction stmt1 stmt2)
    Just (Word "iff") -> do
      stmt2 <- statement
      return (StatementIff stmt1 stmt2)
    -- TODO:
    -- * Add 'where' clauses.
    -- * Add disjunctions. This has a slightly more complicated interaction
    --  with conjunctions. Worst case: parse naively, fix in processing.
    Just (Word "where") -> do
      fail "Parse.Statement.unheadedStatement: where clauses are not implemented yet"
    -- The above options exhaust all cases where a token was consumed.
    -- This is how we proceed when we cannot consume a continuation token.
    _noContinue -> do
      return (StatementAtomic stmt1)
  where
    continue = word "and" <|> iff <|> word "where"

data AtomicStatement
  = Thesis   -- ^ The current goal.
  | Contrary -- ^ Negation of the current goal.
  | Contradiction -- ^ Bottom.
  | SymbolicStatement SymbolicStatement
  | PredicativeAdj Term Adj
  deriving (Show, Eq, Ord)

atomicStatement :: Parser AtomicStatement
atomicStatement = predicativeAdj <|> constStatement <|> (SymbolicStatement <$> symbolicStatement)
  where
    constStatement, thesis, contrary, contradiction :: Parser AtomicStatement
    constStatement = thesis <|> contrary <|> contradiction
    thesis = Thesis <$ try (optional (word "the") *> word "thesis")
    contrary = Contrary <$ try (optional (word "the") *> word "contrary")
    contradiction = Contradiction <$ try (optional (word "a") *> word "contradiction")

    predicativeAdj :: Parser AtomicStatement
    predicativeAdj = do
      n <- term
      word "is"
      adj <- adjective
      return (PredicativeAdj n adj)

data Term
  = TermDefiniteSymbolic Expr
  | TermDefiniteNoun
  | TermQuantified Quantifier Expr
  deriving (Show, Eq, Ord)

term :: Parser Term
term = do
  (quant, noun) <- quantifiedTerm
  return (TermQuantified quant noun)

quantifiedTerm :: Parser (Quantifier, Notion)
quantifiedTerm = label "quantified term" (universal <|> existential <|> nonexistential)
  where
    universal, existential, nonexistential :: Parser (Quantifier, Notion)
    universal = do
      try (word "every")
      noun <- notion
      return (Universal, noun)
    existential = do
      try (word "some")
      noun <- notion
      return (Existential, noun)
    nonexistential = do
      try (word "no")
      noun <- notion
      return (Nonexistential, noun)

type Notion = Expr

notion :: Parser Expr
notion = do
  nominals <- getNominals
  (pat, es) <- patternWith (math expression) nominals
  return (foldl App (ConstPattern pat) es)

adjective :: Parser Adj
adjective = label "adjective" do
  adjs <- getAdjs
  let isAdj t = unLocated t `elem` (Set.map Word adjs)
  result <- satisfy isAdj
  let Word adj = unLocated result
  return adj
