module Language.Expression
  ( module Language.Expression
  , module Language.Common
  ) where


import Language.Common (Var)
import Language.Pattern (Pattern)


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
  | Prop Prop
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
  | Prop `PredApp` Expr
  | Expr `Equals` Expr
  | Expr `And` Expr
  | Expr `Or` Expr
  | Expr `Implies` Expr
  | Forall
  deriving (Show, Eq, Ord)
