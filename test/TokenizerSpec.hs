module TokenizerSpec where


import Test.Hspec
import Test.Hspec.Megaparsec

import Tokenizer


spec :: Spec
spec = do
  parse command "" "\\cmd" `shouldParse` "cmd"