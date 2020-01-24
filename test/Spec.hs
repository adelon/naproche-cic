{-
  Start the test procedure by running `stack test` in
  the project directory. To load the test modules in
  the REPL run `stack repl lore:lore-test`.
-}

import Test.Hspec

import qualified TokenizerSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tokenizer" TokenizerSpec.spec
