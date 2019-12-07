-- Core data type for state information.
--
-- The registry contains all known vocabulary can also keep track of counters.
--

{-# LANGUAGE NamedFieldPuns #-}

module Base.Registry (module Base.Registry, module Export) where


import Base.Parser as Export (Registry(..), initRegistry)

-- Various helper functions

-- | Increments @idCount@ by 1.
incrIdCount :: Registry -> Registry
incrIdCount regis@Registry{idCount} = regis{idCount = succ idCount}
