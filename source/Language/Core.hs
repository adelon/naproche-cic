module Language.Core where


import Language.Expression

import qualified Data.Sequence as Seq


{-


-}

type StructName = Text
type Properties = [Expr]
type StructParam = Typing Var StructName

data StructDecl = StructDecl
   StructName
   [Typing Var StructName] -- ^ Structure parameters
   [Typing Var Expr] -- ^ Structure constants and operations
   [Prop]

newtype Adj = Adj Text deriving (Eq, Show, IsString)

data Phrase
   = Let (Typing Expr Typ)
   | Expr `Is` Adj
   | NamelessForall
   | Phrases (Seq Phrase)

instance Semigroup Phrase where
   (<>) :: Phrase -> Phrase -> Phrase
   Phrases es <> Phrases es' = Phrases (es <> es')
   Phrases es <> e = Phrases (es |> e)
   e <> Phrases es = Phrases (e <| es)
   e <> e' = Phrases (Seq.fromList [e, e'])

instance Monoid Phrase where
   mempty = Phrases mempty
