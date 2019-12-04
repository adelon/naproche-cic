module TokenizerSpec where


import Tokenize

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import Prelude hiding (Word)


spec :: Spec
spec = do
  describe "word" do
    it "works" do
      parse word "" "trivial\n" `shouldParse` Word "trivial"
      parse word "" "Suppose " `shouldParse` Word "suppose"
  describe "command" do
    it "works" do
      parse command "" "\\cmd" `shouldParse` Command "cmd"
  describe "symbol" do
    it "works" do
      parse symbol "" ". " `shouldParse` Symbol "."
      parse symbol "" "@@\n" `shouldParse` Symbol "@@"
  describe "begin" do
    it "works" do
      parse begin "" "\\begin{lemma}\n" `shouldParse` BeginEnv "lemma"
  describe "end" do
    it "works" do
      parse end "" "\\end{theorem}\n" `shouldParse` EndEnv "theorem"
  describe "toks" do
    it "parses an environment with a word" do
      parse toks "" "\\begin{lemma}\nTrivial.\n\\end{lemma}" `shouldParse`
        [BeginEnv "lemma", Word "trivial", Symbol ".", EndEnv "lemma"]