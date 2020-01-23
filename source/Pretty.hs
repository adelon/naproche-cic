{-# OPTIONS_GHC -w #-}

module Pretty where


import Language.Expression
import Language.Pattern
import Parse.Assumption
import Parse.Declaration
import Parse.Definition
import Parse.Document
import Parse.Statement
import Parse.Statement.Symbolic

import Data.Text.Prettyprint.Doc


prettyDocument :: forall ann. Document -> Doc ann
prettyDocument (Document decls) =
  vsep (toList (prettyDeclaration <$> decls))

prettyDeclaration :: forall ann. Declaration -> Doc ann
prettyDeclaration = \case
  DeclAxiom axiom -> prettyAxiom axiom
  DeclDefinition definition -> prettyDefinition definition
  DeclTheorem theorem -> prettyTheorem theorem

prettyAxiom :: forall ann. Axiom -> Doc ann
prettyAxiom (Axiom asms stmt) = vsep
  [ "Axiom:"
  , indent 2 $ prettyAssumptions asms
  , indent 2 $ prettyStatement stmt
  ]

prettyDefinition :: forall ann. Definition -> Doc ann
prettyDefinition (Definition asms bodies) = vsep
  [ "Definition:"
  , indent 2 $ prettyAssumptions asms
  , indent 2 $ prettyDefinitionBodies bodies
  ]

prettyDefinitionBodies :: forall ann. (NonEmpty DefinitionBody) -> Doc ann
prettyDefinitionBodies bodies = vsep (toList (prettyDefinitionBody <$> bodies))

prettyDefinitionBody :: forall ann. DefinitionBody -> Doc ann
prettyDefinitionBody (DefinePredicate head stmt) = vsep
  [ (vsep ["Definiendum:", indent 2 $ prettyHead head])
  , "Definiens:" <+> prettyStatement stmt
  ]
  where
    prettyHead = \case
      PredicateAdjPattern vs pat -> vsep
        [ "Variables:" <+> prettyVarInfo vs
        , "Pattern" <+> prettyPattern pat
        ]
      PredicateVerbPattern vs pat -> vsep
        [ "Variables:" <+> prettyVarInfo vs
        , "Pattern" <+> prettyPattern pat
        ]

prettyVarInfo :: forall ann. NonEmpty (Var, Maybe Typ) -> Doc ann
prettyVarInfo vs = hsep $ punctuate comma $ toList $ prettyVar <$> vs
  where
    prettyVar (v, Nothing) = viaShow v
    prettyVar (v, Just ty) = viaShow v <+> ":" <+> viaShow ty

prettyPattern :: forall ann. Pattern -> Doc ann
prettyPattern shapes = foldr1 (<+>) (prettyShape <$> shapes)
  where
    prettyShape Slot = "_"
    prettyShape (Word ws) = viaShow ws

prettyTheorem :: forall ann. Theorem -> Doc ann
prettyTheorem (Theorem asms stmt) = vsep
  [ "Theorem:"
  , indent 2 $ prettyAssumptions asms
  , indent 2 $ prettyStatement stmt
  ]

prettyAssumptions :: forall ann. [Assumption] -> Doc ann
prettyAssumptions asms = vsep (prettyAssumption <$> asms)

prettyAssumption :: Assumption -> Doc ann
prettyAssumption asm = "Assumption:" <+> case asm of
  AssumptionPretyping vs -> prettyTyping vs
  Assumption stmt -> prettyStatement stmt

prettyTyping :: forall t v ty ann. (Foldable t, Pretty v, Pretty ty)
  => t (Typing v ty) -> Doc ann
prettyTyping vs = foldr alg "" vs
  where
    alg :: Typing v ty -> Doc ann -> Doc ann
    alg (v `Inhabits` ty) pvs = pretty v <+> ":" <> pretty ty <> pvs

prettyStatement :: forall ann. Statement -> Doc ann
prettyStatement = \case
  StatementHeaded hstmt -> case hstmt of
    StatementQuantified quants stmt -> undefined
    StatementImplication stmt1 stmt2 ->
      prettyStatement stmt1 <+> "=>" <+> prettyStatement stmt2
    StatementNegated stmt ->
      "Not:" <+> prettyStatement stmt
  StatementUnheaded uhstmt -> case uhstmt of
    StatementConjunction stmt1 stmt2 ->
      prettyAtomicStatement stmt1 <+> "/\\" <+> prettyStatement stmt2
    StatementDisjunction stmt1 stmt2 ->
      prettyAtomicStatement stmt1 <+> "\\/" <+> prettyStatement stmt2
    StatementIff stmt1 stmt2 ->
      prettyAtomicStatement stmt1 <+> "<=>" <+> prettyStatement stmt2
    StatementImplies stmt1 stmt2 ->
      prettyAtomicStatement stmt1 <+> "=>" <+> prettyStatement stmt2
    StatementWhere stmt whereStmt ->
      prettyAtomicStatement stmt <> "," <+> "where" <+> prettyWhereStatement whereStmt
    StatementAtomic stmt -> prettyAtomicStatement stmt

prettyAtomicStatement :: forall ann. AtomicStatement -> Doc ann
prettyAtomicStatement = \case

  Contradiction -> "A contradiction."
  SymbolicStatement stmt -> prettySymbolicStatement stmt
  PredicativeAdj t (pat, args) -> prettyPredicate pat (t:args)
  PredicativeVerb t (pat, args) -> prettyPredicate pat (t:args)

prettyPredicate :: forall ann. Pattern -> [Term] -> Doc ann
prettyPredicate pat args = prettyPattern pat <> list (prettyTerm <$> args)

prettyTerm :: forall ann. Term -> Doc ann
prettyTerm = \case
  TermDefiniteSymbolic expr -> pretty expr
  TermDefiniteNoun -> "DEFINITE NOUN"
  TermQuantified quant vs expr -> case vs of
    Nothing -> "QUANTIFIED TERM"
    Just vs -> "QUANTIFIED TERM"

prettyWhereStatement :: forall ann. WhereStatement -> Doc ann
prettyWhereStatement whereStmt = undefined

prettySymbolicStatement :: forall ann. SymbolicStatement -> Doc ann
prettySymbolicStatement stmt = undefined
