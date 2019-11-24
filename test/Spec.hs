{-
  Start the test procedure by running `stack test` in
  the project directory. To load the test modules in
  the REPL run `stack repl napcic:napcic-test`.
-}

import Test.Hspec

import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" ParserSpec.spec