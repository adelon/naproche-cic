module Parse.Var (module Parse.Var, module Export) where

import Base.Parser
import Language.Common as Export (Var(..))
import Parse.Token (variable, comma)


var :: Parser Var
var = Var <$> variable
{-# INLINE var #-}

varList :: Parser (NonEmpty Var)
varList = var `sepBy1` comma
{-# INLINE varList #-}
