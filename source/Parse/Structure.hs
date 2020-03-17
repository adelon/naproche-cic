module Parse.Structure where


import Base
import Language.Expression
import Tokenize (Tok(..))

import qualified Data.Map.Strict as Map


-- A structure can be carried by another structure (the 'Just' case),
-- which makes it a structure extension. In this case, the new
-- operations and properties may not overlap with those of the carrier.
-- In the 'Nothing' case, the carrier is a set.
--
-- TODO:
-- * Replace stringly type self-reference.

data Struct = Struct
   { structName :: Text
   , structCarrier :: Maybe Struct
   , structOps :: Map Tok Typ
   , structProps :: [Prop]
   }


-- Here is a magma as a simple example of a structure.
--
magma :: Struct
magma = Struct "magma" Nothing (Map.fromList [(Command "op", binOp)]) []
  where
     binOp = Const "self" `To` Const "self" `To` Const "self"
