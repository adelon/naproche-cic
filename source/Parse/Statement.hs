module Parse.Statement where


import Base.Parser
import Language.Common (Var)
import Language.Expression
import Parse.Expression

import Data.Text (Text)


type Adj = Text


data Statement
  = StatementQuantified [Typing Var Typ] Statement
  | StatementImplication Statement Statement
  | StatementNegated Statement
  | StatementChain Chain
  deriving (Show, Eq)


statement :: Parser Statement
statement = headed <|> chained


headed :: Parser Statement
headed = quantified <|> ifThen <|> negated

  where

    quantified :: Parser Statement
    quantified = do
      vars <- quantifierChain
      stmt <- statement
      return (StatementQuantified vars stmt)

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

quantifierChain :: Parser [Typing Var Typ]
quantifierChain = error "Parse.Statement.quantifierChain incomplete"

quantifiedNotion :: Parser (Typing Var Typ)
quantifiedNotion = label "quantified notion" (universal <|> existential <|> no)
  where
    universal = do
      word "all"
      varInfo <- notion
      -- TODO this needs to be registered as local variable information.
      return varInfo

    existential = error "Parse.Statement.quantifiedNotion incomplete"

    no = error "Parse.Statement.quantifiedNotion incomplete"


chained :: Parser Statement
chained = StatementChain <$> (andOrChain <|> neitherNorChain)
    where

      andOrChain :: Parser Chain
      andOrChain = error "Parse.Statement.andOrChain incomplete"

      neitherNorChain :: Parser Chain
      neitherNorChain = error "Parse.Statement.neitherNorChain incomplete"


data Chain
  = Chain
  deriving (Show, Eq, Ord)

notion :: Parser (Typing Var Typ)
notion = notionExpr
  where
    notionExpr = math typing


atomic :: Parser Statement
atomic = error "Parse.Statement.atomic incomplete"
