{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module Main where


import Base.Parser
import Base.Registry (initRegistry)
import Parse.Document
import Parse.Token (TokStream(..))
import Tokenize

import Control.Monad.State.Strict
import Data.List ((\\))
import Data.Text (Text)
import Data.Void
import System.Directory
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  files <- getFiles "work/in"
  mapM_ work files


getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  contents <- getDirectoryContents dir
  let contents' = contents \\ [".",".."]
  return contents'


work :: FilePath -> IO ()
work file = do
  let inPath = "./work/in/" <> file
  let outPath = "./work/out/" <> file
  let debugPath = "./work/out/" <> file <> ".tokens"
  result <- tokenize inPath
  case result of
    Left err -> Text.writeFile debugPath (Text.pack (errorBundlePretty err))
    Right stream -> do
      Text.writeFile debugPath (dumpTokens stream)
      result' <- parse document inPath stream
      case result' of
        Left err -> Text.writeFile outPath (Text.pack (errorBundlePretty err))
        Right doc -> Text.writeFile outPath ((pack . show) doc)


dumpTokens :: TokStream -> Text
dumpTokens = pack . show . fmap tokenVal . unTokStream

tokenize :: FilePath -> IO (Either (ParseErrorBundle Text Void) TokStream)
tokenize path = do
  raw <- Text.readFile path
  let result = runParser toks path raw
  case result of
    Left err -> return (Left err)
    Right stream -> return (Right (TokStream raw stream))


parse :: Parser a -> String -> TokStream -> IO (Either (ParseErrorBundle TokStream Void) a)
parse p path stream = do
  let (result, _finalState) = runState (runParserT p path stream) initRegistry
  return result
