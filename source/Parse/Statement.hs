module Parse.Statement where


import Base.Parser
import Language.Common (Var)
import Language.Expression
import Language.Quantifier
import Parse.Expression (expression, typing)
import Parse.Pattern (Pattern, patternWith)
import Parse.Token
import Parse.Var (varList)


import qualified Data.Map.Strict as Map

type Statement = Prop

statement :: Parser Prop
statement = headedStatement <|> unheadedStatement


headedStatement :: Parser Prop
headedStatement = quantified <|> ifThen <|> negated
   where
   quantified :: Parser Prop
   quantified = do
      quantify <- quantifierChain
      optional comma
      optional (word "we" >> word "have" >> optional (word "that"))
      stmt <- statement
      return (quantify stmt)

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
      return (stmt1 `Implies` stmt2)


-- Parses a quantification returning a quantification function.
--
quantifierChain :: Parser (Prop -> Prop)
quantifierChain = do
   (quant, vs) <- quantifiedNominal
   let quantifications = (\(v `Inhabits` ty) -> Quantify quant v ty) <$> vs
--
--                     TODO: Check that this is actually the correct
--                     order of composition for quantifications!
--                     (This matters obviously, as they do not commute!)
--
--                     This takes a list of function [a -> a] and a starting value
--                     and applies all the functions to that starting value.
--                     vvvvvvv
   let quantify prop = compose quantifications prop
   return quantify


quantifiedNominal :: Parser (Quantifier, NonEmpty (Typing Var Typ))
quantifiedNominal = label "quantified nominal" (universal <|> nonexistential <|> existential)
   where
   universal, existential, nonexistential :: Parser (Quantifier, NonEmpty (Typing Var Typ))
   universal = do
      word "all" <|> try (word "for" >> (word "every" <|> word "all"))
      vs <- varInfo
      -- TODO this needs to be registered as local variable information.
      optional (word "we" >> word "have" >> word "that")
      return (Universal, vs)
   nonexistential = do
      try (thereExists >> word "no")
      vs <- varInfo
      optional suchThat
      -- TODO this needs to be registered as local variable information.
      return (Nonexistential, vs)
   existential = do
      thereExists
      optional (word "a")
      vs <- varInfo
      optional suchThat
      -- TODO this needs to be registered as local variable information.
      return (Existential, vs)
   varInfo :: Parser (NonEmpty (Typing Var Typ))
   varInfo = nominalInfo <|> symbolicInfo
      where
      nominalInfo = do
         (pat, ts) <- nominal
         let ty = (foldl App (ConstPattern pat) ts)
         vs <- math varList
         return ((`Inhabits` ty) <$> vs)
      symbolicInfo = math typing


unheadedStatement :: Parser Prop
unheadedStatement = do
   stmt1 <- atomicStatement
   peeking <- optional continue
   case peeking of
      Just (Word "and") -> do
         stmt2 <- statement
         return (stmt1 `And` stmt2)
      Just (Word "or") -> do
         stmt2 <- statement
         return (stmt1 `Or` stmt2)
      Just (Word "iff") -> do
         stmt2 <- statement
         return (stmt1 `Or` stmt2)
      Just (Word "implies") -> do
         optional (word "that")
         stmt2 <- statement
         return (stmt1 `Implies` stmt2)
      -- Just (Word "where") -> do
      --   info <- whereStatement
      --   return (StatementWhere stmt1 info)
      _otherwise -> pure stmt1
   where
   continue :: Parser Tok
   continue = word "and" <|> word "or" <|> iff <|> word "where" <|> word "implies"

   -- whereStatement :: Parser WhereStatement
   -- whereStatement = sepByComma1 do
   --   begin "math"
   --   v <- var
   --   symbol "=" <|> (end "math" *> word "is" *> begin "math")
   --   expr <- expression
   --   end "math"
   --   return (v, expr)


atomicStatement :: Parser Prop
atomicStatement = predication <|> contradiction <|> symbolicStatement
   where
   contradiction :: Parser Prop
   contradiction = Falsum <$ try (optional (word "a") *> word "contradiction")

   predication :: Parser Prop
   predication = do
      n <- try term
      peeking <- optional continue
      case peeking of
         Just (Word "is") -> do
            (pat, args) <- adjective
            pure (patternPredication pat (n : args))
         _otherwise -> do
            (pat, args) <- verb
            pure (patternPredication pat (n : args))
      where
         continue :: Parser Tok
         continue = word "is"

-- TODO: implement quantified terms.
-- They could be implemented as functions that get applied when desugaring and
-- quantify their surrounding expression.

term :: Parser (Expr)
term = math expression

type Nominal = (Pattern, [Expr])

nominal :: Parser Nominal
nominal = do
   pats <- getNominals
   patternWith term pats

type Adj = (Pattern, [Expr])

adjective :: Parser Adj
adjective = label "adjective" do
   pats <- getAdjs
   patternWith term pats


type Verb = (Pattern, [Expr])

verb :: Parser Verb
verb = label "verb" do
   pats <- getVerbs
   patternWith term pats


symbolicStatement :: Parser Prop
symbolicStatement = label "symbolic statement" $ math relatorChain

relatorChain :: Parser Prop
relatorChain = do
   expr <- toList <$> expression `sepBy1` comma
   ch <- many chain
   return (makeChain (expr) ch)
   where
      chain :: Parser (Relator, [Expr])
      chain = do
         rel <- relator
         expr <- toList <$> expression `sepBy1` comma
         return (rel, expr)

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
