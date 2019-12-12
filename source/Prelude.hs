{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
  ( module Export
  ) where

import BasePrelude as Export hiding (Word, TypeError, Const, try, some, many, pi, app)

import Data.Map.Strict as Export (Map)
import Data.Sequence as Export (Seq(..), (<|), (|>))
import Data.Sequence1 as Export (Seq1(..))
import Data.Set as Export (Set)
import Data.Text as Export (Text)
import Data.Tree as Export (Tree(..))