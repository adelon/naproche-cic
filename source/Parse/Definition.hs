module Parse.Definition where


import Base.Parser
import Parse.Assumption (Assumption, assumption)
import Parse.Expression (Typ, varInfo)
import Parse.Pattern (Pattern, anyPatternBut)
import Parse.Statement (Statement, statement)
import Parse.Token
import Parse.Var (Var, var)

import qualified Data.Set as Set


data Definition
   = Definition [Assumption] (NonEmpty DefinitionBody)
   deriving (Show, Eq)

definition :: Parser Definition
definition = environment "definition" do
   asms <- many assumption
   body <- many1 definitionBody
   return (Definition asms body)

data DefinitionBody
   = DefinePredicate PredicateHead Statement
   deriving (Show, Eq)

definitionBody :: Parser DefinitionBody
definitionBody = do
   optional weSay
   head <- predicateHead
   iff
   stmt <- statement `endedBy` period
   return (DefinePredicate head stmt)
   where
   weSay :: Parser ()
   weSay = void (try (word "we" *> word "say") *> optional (word "that"))

data PredicateHead
   = PredicateAdjPattern (NonEmpty (Var, Maybe Typ)) Pattern
   | PredicateVerbPattern (NonEmpty (Var, Maybe Typ)) Pattern
   | PredicateNominalPattern (NonEmpty (Var, Maybe Typ)) Pattern
   | PredicateRelator (Var, Text, Var)
   deriving (Show, Eq)

predicateHead :: Parser PredicateHead
predicateHead = patterned <|> relator
   where
   patterned :: Parser PredicateHead
   patterned = do
      -- TODO: nominals, x-of-y
      v <- try (math varInfo)
      maybeCopula <- optional copula
      case maybeCopula of
         Just (Word "is") -> do
            maybeIndefinite <- optional indefinite
            case maybeIndefinite of
               Just (Word "an") -> do
                  (pat, vs) <- anyPatternBut (Set.fromList ["if", "iff"])
                  trace ("registering:" <> show pat) $ registerNominal pat
                  let vars = v :| vs
                  return (PredicateNominalPattern vars pat)
               _otherwise -> do
                  (pat, vs) <- anyPatternBut (Set.fromList ["if", "iff"])
                  registerAdj pat
                  let vars = v :| vs
                  return (PredicateAdjPattern vars pat)
         _otherwise -> do
            (pat, vs) <- anyPatternBut (Set.fromList ["if", "iff"])
            registerVerb pat
            let vars = v :| vs
            return (PredicateVerbPattern vars pat)
      where
      copula, indefinite :: Parser Tok
      copula = word "is"
      indefinite = (word "a" <|> word "an") >> pure (Word "an")

   relator :: Parser PredicateHead
   relator = PredicateRelator <$> math do
      x1 <- var
      rel <- anyCommand
      x2 <- var
      return (x1, rel, x2)
