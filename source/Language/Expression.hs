module Language.Expression
   ( module Language.Expression
   , module Language.Common
   ) where


import Base
import Language.Common (Var)
import Language.Pattern (Pattern)
import Language.Quantifier (Quantifier(..))
import Tokenize (Tok)

import Data.Text.Prettyprint.Doc
import qualified Data.Set as Set


type Typ = Expr

data Typing a b = a `Inhabits` b deriving (Show, Eq, Ord)
data Proof a b = a `Proves` b deriving (Show, Eq, Ord)

data Expr
   = Hole
   | Const Text
   | ConstPattern Pattern
   | Bound Natural
   | Free Var
   | Bottom
   | Top
   | Expr `Times` Expr -- Product
   | Expr `Plus` Expr -- Coproduct
   | Expr `To` Expr -- Exponential
   | Pi Var Typ Expr
   | Sigma Var Typ Expr
   | Lambda Var Typ Expr
   | Expr `App` Expr
   | Array [Expr]
   deriving (Show, Eq, Ord)

infixr 4 `To`
infixl 6 `App`

freeVariables :: Expr -> Set.Set Var
freeVariables = go mempty
  where
    go fv = \case
      (Free v) -> Set.insert v fv
      (e1 `Times` e2) -> go (go fv e1) e2
      (e1 `Plus` e2) -> go (go fv e1) e2
      (e1 `To` e2) -> go (go fv e1) e2
      (e1 `App` e2) -> go (go fv e1) e2
      (Array es) -> foldl' go fv es
      (Pi v t e) -> go (Set.union fv (Set.delete v $ go mempty e)) t
      (Sigma v t e) -> go (Set.union fv (Set.delete v $ go mempty e)) t
      (Lambda v t e) -> go (Set.union fv (Set.delete v $ go mempty e)) t
      _ -> fv

data Prop
   = Falsum
   | Verum
   | Squashed Expr
   | Predicate Text
   | Rel Tok
   | PredicatePattern Pattern
   | Not Prop
   | Prop `PredApp` Expr
   | Expr `Equals` Expr
   | Prop `And` Prop
   | Prop `Or` Prop
   | Prop `Implies` Prop
   | Quantify Quantifier Var Typ Prop
   deriving (Show, Eq, Ord)

freeVariablesProp :: Prop -> Set.Set Var
freeVariablesProp = go mempty
  where
    go fv = \case
      (Squashed e) -> Set.union fv (freeVariables e)
      (Not p) -> go fv p
      (p `PredApp` e) -> go (Set.union fv (freeVariables e)) p
      (e1 `Equals` e2) -> Set.unions [freeVariables e1, freeVariables e2, fv]
      (p1 `And` p2) -> go (go fv p1) p2
      (p1 `Or` p2) -> go (go fv p1) p2
      (p1 `Implies` p2) -> go (go fv p1) p2
      (Quantify _ v t p) -> Set.unions [fv, Set.delete v $ go mempty p, freeVariables t]
      _ -> fv

patternPredication :: Pattern -> [Expr] -> Prop
patternPredication pat args = foldl PredApp (PredicatePattern pat) args


instance Pretty Expr where
   pretty = viaShow
