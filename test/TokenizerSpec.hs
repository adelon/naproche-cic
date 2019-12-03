module TokenizerSpec where


import Tokenize

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)
 

spec :: Spec
spec = do
  describe "command" do
    it "works" do  
      parse command "" "\\cmd" `shouldParse` Command "cmd"