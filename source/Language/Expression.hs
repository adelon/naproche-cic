module Language.Expression
  ( module Language.Expression
  , module Language.Common
  ) where


import Language.Common (Var)
import Language.Pattern (Pattern)
import Language.Quantifier (Quantifier(..))
import Tokenize (Tok)

import Data.Text.Prettyprint.Doc


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


-- quantify = undefined

patternPredication :: Pattern -> [Expr] -> Prop
patternPredication pat args = foldl PredApp (PredicatePattern pat) args


instance Pretty Expr where
  pretty = viaShow
