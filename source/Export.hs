module Export where


import Base
import Base.Parser (Registry(..), incrVar)
import Language.Expression
import Language.Pattern
import Language.Lean (preamble)
import Parse.Assumption (Assumption(..))
import Parse.Declaration (Declaration(..), Axiom(..), Theorem(..))
import Parse.Definition (Definition(..), DefinitionBody(..), PredicateHead(..))
import Parse.Document (Document(..))
import Parse.Statement

import Control.Monad.Reader
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text


export :: Document -> Registry -> Text
export doc regis = runReader exporting regis
   where
   exporting :: Reader Registry Text
   exporting = renderStrict <$> layoutCompact <$> do
      doc' <- exportDocument doc
      pure $ vsep [pretty preamble, doc']

exportDocument :: forall ann. Document -> Reader Registry (Doc ann)
exportDocument (Document decls) = vsep <$> toList <$> traverse exportDeclaration decls


exportDeclaration :: forall ann. Declaration -> Reader Registry (Doc ann)
exportDeclaration = \case
   DeclAxiom axiom -> exportAxiom axiom
   DeclDefinition definition -> exportDefinition definition
   DeclTheorem theorem -> undefined --exportTheorem' theorem
   DeclRemark _remark -> pure ""


exportAxiom :: forall ann. Axiom -> Reader Registry (Doc ann)
exportAxiom (Axiom name asms stmt) = do
   asms' <- exportAssumptions' asms
   stmt' <- exportProp' stmt
   freshId <- local incrVar (asks varCount) -- Grabs a fresh id.
   let defaultName = "axiom_" <> pretty freshId
   let name' = fromMaybe defaultName (pretty <$> name)
   pure $ hsep ["axiom", name', asms', ":", stmt'] <> line


exportDefinition :: forall ann. Definition -> Reader Registry (Doc ann)
exportDefinition (Definition asms bodies) = vsep <$> toList <$> do
   asms' <- exportAssumptions' asms
   let
      trafo :: DefinitionBody -> Reader Registry (Doc ann)
      trafo body = do
         body' <- exportDefinitionBody' body
         pure $ hsep ["def", nameFromBody body, asms', ":=", body'] <> line
   traverse trafo bodies

nameFromBody :: forall ann. DefinitionBody -> Doc ann
nameFromBody (DefinePredicate head _stmt) = case head of
   PredicateAdjPattern _info pat     -> pretty $ makeInterpretation pat
   PredicateVerbPattern _info pat    -> pretty $ makeInterpretation pat
   PredicateNominalPattern _info pat -> pretty $ makeInterpretation pat
   PredicateRelator (_v1, rel, _v2)  -> pretty $ rel -- replace with lookup in registry.


exportTheorem :: forall ann. Theorem -> Reader Registry (Doc ann)
exportTheorem (Theorem _name asms stmt) = do
   asms' <- exportAssumptions' asms
   stmt' <- exportProp' stmt
   let proof = "omitted"
   pure $ hsep ["theorem", "theorem_name_TODO", asms', ":", stmt', ":=", proof] <> line







exportAssumptions' :: forall ann. [Assumption] -> Reader Registry (Doc ann)
exportAssumptions' asms = undefined asms --vsep (exportAssumption <$> asms)

exportDefinitionBody' :: forall ann. DefinitionBody -> Reader Registry (Doc ann)
exportDefinitionBody' body = undefined body



exportProp' :: forall ann. Prop -> Reader Registry (Doc ann)
exportProp' = undefined


------------------------



exportDefinitionBody :: forall ann. DefinitionBody -> Doc ann
exportDefinitionBody (DefinePredicate head stmt) = vsep
   [ (vsep ["Definiendum:", indent 2 $ exportHead head])
   , "Definiens:" <+> exportProp stmt <> line
   ]
   where
   exportHead = \case
      PredicateAdjPattern vs pat -> vsep
         [ "Variables:" <+> exportVarInfo vs
         , "Pattern:" <+> "<?>_is_" <> pretty pat
         ]
      PredicateVerbPattern vs pat -> vsep
         [ "Variables:" <+> exportVarInfo vs
         , "Pattern:" <+> "<?>_" <> pretty pat
         ]
      PredicateNominalPattern vs pat -> vsep
         [ "Variables:" <+> exportVarInfo vs
         , "Pattern:" <+> "<?>_is_a_" <> pretty pat
         ]
      PredicateRelator (a, rel, b) -> vsep
         [ "Variables:" <+> pretty a <+> "and" <+> pretty b
         , "Pattern:" <+> pretty rel
         ]

exportVarInfo :: forall ann. NonEmpty (Var, Maybe Typ) -> Doc ann
exportVarInfo vs = hsep $ punctuate comma $ toList $ exportVar <$> vs
   where
   exportVar (v, Nothing) = pretty v
   exportVar (v, Just ty) = pretty v <+> ":" <+> exportExpr ty



exportAssumption :: Assumption -> Doc ann
exportAssumption asm = "Assumption:" <+> case asm of
   AssumptionPretyping vs -> exportTyping vs
   Assumption stmt -> exportProp stmt

exportTyping :: forall t v ty ann. (Foldable t, Pretty v, Pretty ty)
  => t (Typing v ty) -> Doc ann
exportTyping vs = foldr alg "" vs
   where
   alg :: Typing v ty -> Doc ann -> Doc ann
   alg (v `Inhabits` ty) pvs = pretty v <+> ":" <> pretty ty <> pvs

exportProp :: forall ann. Prop -> Doc ann
exportProp = \case
   Falsum -> "false"
   Verum -> "true"
   Squashed e -> "(nonempty" <+> exportExpr e <> ")"
   Predicate p -> pretty p
   Rel tok -> viaShow tok
   PredicatePattern pat -> pretty pat
   Not p -> "¬" <+> exportProp p
   p `PredApp` e -> "(" <> exportProp p <+> exportExpr e <> ")"
   e1 `Equals` e2 -> "(" <> exportExpr e1 <+> "=" <+> exportExpr e2 <> ")"
   p `And` q -> "(" <> exportProp p <+> "∧" <+> exportProp q <> ")"
   p `Or` q -> "(" <> exportProp p <+> "∨" <+> exportProp q <> ")"
   p `Implies` q -> "(" <> exportProp p <+> "->" <+> exportProp q <> ")"
   Quantify quant v ty p ->
      pretty quant <+> pretty v <> "," <+> exportExpr ty <> exportProp p



exportExpr :: forall ann. Expr -> Doc ann
exportExpr = \case
   Hole -> "_"
   Const c -> pretty c
   ConstPattern pat -> pretty pat
   Bottom -> "∅"
   Top -> "*"
   Free v -> pretty v
   e1 `Times` e2 -> "(" <> exportExpr e1 <+> "×" <+> exportExpr e2 <> ")"
   e1 `Plus` e2  -> "(" <> exportExpr e1 <+> "+" <+> exportExpr e2 <> ")"
   e1 `To` e2    -> "(" <> exportExpr e1 <+> "->" <+> exportExpr e2 <> ")"
   e1 `App` e2 -> "(" <> exportExpr e1 <+> exportExpr e2 <> ")"
   _ -> error "missing case in exportExpr"
