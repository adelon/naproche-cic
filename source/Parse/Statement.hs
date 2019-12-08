module Parse.Statement where


import Base.Parser
import Language.Common (Var)
import Language.Expression
import Parse.Expression
import Parse.Token
import Tokenize (Tok(..), Located(..))

import Data.Text (Text)

import qualified Data.Set as Set

type Adj = Text


data Statement
  = StatementQuantified [(Quantifier, Typing Var Typ)] Statement
  | StatementImplication Statement Statement
  | StatementNegated Statement
  | StatementChain Chain
  -- v TODO: remove, this is just for prototyping.
  | AtomicStatement AtomicStatement
  deriving (Show, Eq)

data Quantifier
  = Universal
  | Existential
  | Nonexistential
  deriving (Show, Eq, Ord)

statement :: Parser Statement
statement = AtomicStatement <$> atomicStatement


headed :: Parser Statement
headed = quantified <|> ifThen <|> negated

  where

    quantified :: Parser Statement
    quantified = do
      info <- quantifierChain
      stmt <- statement
      return (StatementQuantified info stmt)

    negated :: Parser Statement
    negated = do
      word "it" *> word "is" *> word "not" *> word "the" *> word "case" *> word "that"
      StatementNegated <$> statement

    ifThen :: Parser Statement
    ifThen = do
      word "if"
      stmt1 <- statement
      optional comma
      word "then"
      stmt2 <- statement
      return (StatementImplication stmt1 stmt2)

quantifierChain :: Parser [(Quantifier, Typing Var Typ)]
quantifierChain = error "Parse.Statement.quantifierChain incomplete"

quantifiedNotion :: Parser (Quantifier, Typing Var Typ)
quantifiedNotion = label "quantified notion" (universal <|> existential <|> nonexistential)
  where
    universal, existential, nonexistential :: Parser (Quantifier, Typing Var Typ)
    universal = do
      word "all"
      varInfo <- undefined
      -- TODO this needs to be registered as local variable information.
      return (Universal, varInfo)
    existential = do
      word "some"
      varInfo <- undefined
      -- TODO this needs to be registered as local variable information.
      return (Existential, varInfo)
    nonexistential = do
      word "no"
      varInfo <- undefined
      -- TODO this needs to be registered as local variable information.
      return (Nonexistential, varInfo)


chained :: Parser Statement
chained = StatementChain <$> (andOrChain <|> neitherNorChain)
    where

      andOrChain :: Parser Chain
      andOrChain = error "Parse.Statement.andOrChain incomplete"

      neitherNorChain :: Parser Chain
      neitherNorChain = error "Parse.Statement.neitherNorChain incomplete"


data Chain
  = Chain
  | End AtomicStatement
  deriving (Show, Eq, Ord)





data AtomicStatement
  = Thesis   -- ^ The current goal.
  | Contrary -- ^ Negation of the current goal.
  | Contradiction -- ^ Bottom.
  | SymbolicStatement Prop
  | PredicativeAdj Term Adj
  deriving (Show, Eq, Ord)

atomicStatement :: Parser AtomicStatement
atomicStatement = predicativeAdj <|> constStatement
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
  | TermQuantified Quantifier Notion
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

type Notion = Text

notion :: Parser Notion
notion = do
  notions <- getNotions
  let isNotion t = tokenVal t `elem` (Set.map Word notions)
  result <- satisfy isNotion
  let Word noun = tokenVal result
  return noun

adjective :: Parser Adj
adjective = label "adjective" do
  adjs <- getAdjs
  let isAdj t = tokenVal t `elem` (Set.map Word adjs)
  result <- satisfy isAdj
  let Word adj = tokenVal result
  return adj