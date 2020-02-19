module Pretty where


import Base
import Language.Expression
import Parse.Assumption (Assumption(..))
import Parse.Declaration (Declaration(..), Axiom(..), Theorem(..))
import Parse.Definition (Definition(..), DefinitionBody(..), PredicateHead(..))
import Parse.Document (Document(..))

import Data.Text.Prettyprint.Doc


prettyDocument :: forall ann. Document -> Doc ann
prettyDocument (Document decls) =
   vsep (toList ((\decl -> prettyDeclaration decl <> line) <$> decls))

prettyDeclaration :: forall ann. Declaration -> Doc ann
prettyDeclaration = \case
   DeclAxiom axiom -> prettyAxiom axiom
   DeclDefinition definition -> prettyDefinition definition
   DeclTheorem theorem -> prettyTheorem theorem
   DeclRemark _remark -> "<remark>"

prettyAxiom :: forall ann. Axiom -> Doc ann
prettyAxiom (Axiom asms stmt) = vsep
   [ "Axiom:"
   , indent 2 $ prettyAssumptions asms
   , indent 2 $ prettyProp stmt
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
   , "Definiens:" <+> prettyProp stmt <> line
   ]
   where
   prettyHead = \case
      PredicateAdjPattern vs pat -> vsep
         [ "Variables:" <+> prettyVarInfo vs
         , "Pattern:" <+> "<?>_is_" <> pretty pat
         ]
      PredicateVerbPattern vs pat -> vsep
         [ "Variables:" <+> prettyVarInfo vs
         , "Pattern:" <+> "<?>_" <> pretty pat
         ]
      PredicateNominalPattern vs pat -> vsep
         [ "Variables:" <+> prettyVarInfo vs
         , "Pattern:" <+> "<?>_is_a_" <> pretty pat
         ]
      PredicateRelator (a, rel, b) -> vsep
         [ "Variables:" <+> pretty a <+> "and" <+> pretty b
         , "Pattern:" <+> pretty rel
         ]

prettyVarInfo :: forall ann. NonEmpty (Var, Maybe Typ) -> Doc ann
prettyVarInfo vs = hsep $ punctuate comma $ toList $ prettyVar <$> vs
   where
   prettyVar (v, Nothing) = pretty v
   prettyVar (v, Just ty) = pretty v <+> ":" <+> prettyExpr ty

prettyTheorem :: forall ann. Theorem -> Doc ann
prettyTheorem (Theorem asms stmt) = vsep
   [ "Theorem:"
   , indent 2 $ prettyAssumptions asms
   , indent 2 $ prettyProp stmt
   ]

prettyAssumptions :: forall ann. [Assumption] -> Doc ann
prettyAssumptions asms = vsep (prettyAssumption <$> asms)

prettyAssumption :: Assumption -> Doc ann
prettyAssumption asm = "Assumption:" <+> case asm of
   AssumptionPretyping vs -> prettyTyping vs
   Assumption stmt -> prettyProp stmt

prettyTyping :: forall t v ty ann. (Foldable t, Pretty v, Pretty ty)
  => t (Typing v ty) -> Doc ann
prettyTyping vs = foldr alg "" vs
   where
   alg :: Typing v ty -> Doc ann -> Doc ann
   alg (v `Inhabits` ty) pvs = pretty v <+> ":" <> pretty ty <> pvs

prettyProp :: forall ann. Prop -> Doc ann
prettyProp = \case
   Falsum -> "F"
   Verum -> "T"
   Squashed e -> "|" <> prettyExpr e <> "|"
   Predicate p -> pretty p
   Rel tok -> viaShow tok
   PredicatePattern pat -> pretty pat
   Not p -> "¬" <+> prettyProp p
   p `PredApp` e -> "(" <> prettyProp p <+> prettyExpr e <> ")"
   e1 `Equals` e2 -> "(" <> prettyExpr e1 <+> "=" <+> prettyExpr e2 <> ")"
   p `And` q -> "(" <> prettyProp p <+> "/\\" <+> prettyProp q <> ")"
   p `Or` q -> "(" <> prettyProp p <+> "\\/" <+> prettyProp q <> ")"
   p `Implies` q -> "(" <> prettyProp p <+> "=>" <+> prettyProp q <> ")"
   Quantify quant v ty p ->
      pretty quant <+> pretty v <> ":" <> prettyExpr ty <> prettyProp p



prettyExpr :: forall ann. Expr -> Doc ann
prettyExpr = \case
   Hole -> "[?]"
   Const c -> pretty c
   ConstPattern pat -> pretty pat
   Bottom -> "∅"
   Top -> "*"
   Free v -> pretty v
   e1 `Times` e2 -> "(" <> prettyExpr e1 <+> "×" <+> prettyExpr e2 <> ")"
   e1 `Plus` e2  -> "(" <> prettyExpr e1 <+> "+" <+> prettyExpr e2 <> ")"
   e1 `To` e2    -> "(" <> prettyExpr e1 <+> "->" <+> prettyExpr e2 <> ")"
   e1 `App` e2 -> "(" <> prettyExpr e1 <+> prettyExpr e2 <> ")"
   _ -> error "missing case in prettyExpr"
