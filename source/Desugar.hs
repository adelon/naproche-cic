module Desugar where


import Language.Expression (Expr(..))

import qualified Language.Surface as Surf
import qualified Language.Core as Core


desugar :: Surf.Phrase -> Core.Phrase
desugar = \case

  Surf.Let typing -> Core.Let typing

  -- | Translation of simple adjectival statements.
  e `Surf.Is` adj
    -> e `Core.Is` Core.Adj (Surf.unAdj adj)
  es `Surf.Are` Surf.DistributiveAdj adj
    -> fold ((`Core.Is` Core.Adj adj) <$> es)
  es `Surf.Are` Surf.CollectiveAdj adj
    -> Array es `Core.Is` Core.Adj adj

  _ -> error "Desugar.desugar incomplete."
