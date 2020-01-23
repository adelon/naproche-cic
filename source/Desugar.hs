module Desugar where


import Language.Expression
import Language.Pattern
import Language.Quantifier (Quantifier(..))
import Parse.Assumption
import Parse.Declaration
import Parse.Definition
import Parse.Document
import Parse.Statement
import Parse.Statement.Symbolic

todo :: forall a. a
todo = error "desugar"

desugarStatement :: Statement -> Prop
desugarStatement = \case
  StatementHeaded stmt -> desguarHeadedStatement stmt
  StatementUnheaded stmt -> desguarStatementUnheaded stmt

desguarHeadedStatement :: HeadedStatement -> Prop
desguarHeadedStatement = \case
  StatementQuantified quants stmt ->
    unrollQuants quants (desugarStatement stmt)
  StatementImplication stmt1 stmt2 ->
    desugarStatement stmt1 `Implies` desugarStatement stmt2
  StatementNegated stmt -> Not (desugarStatement stmt)

unrollQuants :: Foldable t => t (Quantifier, Typing Var Typ) -> Prop -> Prop
unrollQuants quants stmt0 =
  foldr (\(quant, v `Inhabits` ty) stmt -> Quantify quant v ty stmt) stmt0 quants

desguarStatementUnheaded :: UnheadedStatement -> Prop
desguarStatementUnheaded = \case
  StatementConjunction stmt1 stmt2 ->
    desugarAtomicStatement stmt1 `And` desugarStatement stmt2
  StatementDisjunction stmt1 stmt2 ->
    desugarAtomicStatement stmt1 `Or` desugarStatement stmt2
  StatementIff stmt1 stmt2 ->
    (desugarAtomicStatement stmt1 `Implies` desugarStatement stmt2) `And`
    (desugarStatement stmt2 `Implies` desugarAtomicStatement stmt1)
  StatementImplies stmt1 stmt2 ->
    desugarAtomicStatement stmt1 `Implies` desugarStatement stmt2
  StatementWhere _stmt _whereStmt ->
    error "desguarStatementUnheaded: where statements are not fully implemented"
  StatementAtomic stmt -> desugarAtomicStatement stmt

desugarAtomicStatement :: AtomicStatement -> Prop
desugarAtomicStatement = \case
  Contradiction -> Falsum
  SymbolicStatement stmt -> desguarSymbolicStatement stmt
  PredicativeAdj t (adj, ts) ->
    foldl PredApp (PredicatePattern adj) (desugarTerm <$> (t:ts))
  PredicativeVerb t (vrb, ts) ->
    foldl PredApp (PredicatePattern vrb) (desugarTerm <$> (t:ts))

desguarSymbolicStatement :: SymbolicStatement -> Prop
desguarSymbolicStatement (RelatorChain ch) = todo

hoistIndefinites :: NonEmpty Term -> ([(Quantifier, Typing Var Typ)], NonEmpty Term)
hoistIndefinites (TermQuantified quants mvs (noun, args) :| ts) = todo


desugarTerm :: Term -> Expr
desugarTerm = \case
   TermDefiniteSymbolic e -> e
   TermDefiniteNoun -> todo
   TermQuantified quants mvs e -> todo
-- ^^^^^^^^^^^^^^
-- TODO: These need to be hoisted up to make the quantification work.
