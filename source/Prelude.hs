{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
   ( module Export
   , compose
   , plural
   ) where

import BasePrelude as Export hiding
   ( Word, TypeError, Const
   , (<+>)
   , head, last, init, tail, lines
   , try, some, many
   , words, pi, app
   )

import Data.Map.Strict as Export (Map)
import Data.Sequence as Export (Seq(..), (<|), (|>))
import Data.Sequence1 as Export (Seq1(..), pattern IsSeq1, pattern IsEmpty)
import Data.Set as Export (Set)
import Data.Set1 as Export (Set1)
import Data.Text as Export (Text)
import Data.Tree as Export (Tree(..))
import Data.Text.Prettyprint.Doc (plural)

compose :: Foldable f => f (a -> a) -> a -> a
compose = foldr (.) id
