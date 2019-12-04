module Language.Surface where


import Language.Expression

import qualified Data.Sequence as Seq

import Data.Sequence (Seq, (<|), (|>))
import Data.Text (Text)


{-

A structure declaration represents sentences such as

```
A preorder is a set P equipped with a reflexive transitive endorelation ≤.
```
or simply
```
A preorder is a set equipped with a reflexive transitive endorelation.
```

Special importance is placed on the carrier of a structure. When `S` is a structure,
then `x : S` is elaborated to `x : X`, where `X` is the carrier of `S`.

The ingredients of a structure are things like special values (e.g. a neutral element),
operations (e.g. a group operation), relations, or other data (e.g. a topology).

Properties of a structures are proposition that an structures and its ingredients have
to satisfy. For example, we might require an operation to be associative or
a relation to be reflexive.

-}

type StructName = Text
type Properties = [Expr]
type StructParam = Typing Var StructName

data Adj
  = DistributiveAdj Text
  | CollectiveAdj Text
  deriving (Eq, Show)

unAdj :: Adj -> Text
unAdj = \case
  DistributiveAdj adj -> adj
  CollectiveAdj adj -> adj

data Phrase
  = Let (Typing Expr Typ)
  | Expr `Is` Adj
  | [Expr] `Are` Adj
  | Phrases (Seq Phrase)

instance Semigroup Phrase where
  (<>) :: Phrase -> Phrase -> Phrase
  Phrases es <> Phrases es' = Phrases (es <> es')
  Phrases es <> e = Phrases (es |> e)
  e <> Phrases es = Phrases (e <| es)
  e <> e' = Phrases (Seq.fromList [e, e'])

instance Monoid Phrase where
  mempty = Phrases mempty

data StructDecl
  = StructFresh
    StructName
  | StructExtension
    StructName
  | StructCurtailment
    StructName