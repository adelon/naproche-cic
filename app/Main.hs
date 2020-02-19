{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module Main where

import Base
import Base.Parser
import Parse.Document
import Parse.Token (TokStream(..))
import Pretty
import Tokenize

import Control.Monad.State.Strict
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import System.IO (IOMode(WriteMode), withFile)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  let createParents = True
  createDirectoryIfMissing createParents "work/out"
  files <- getFiles "work/in"
  mapM_ work files


getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  contents <- getDirectoryContents dir
  let contents' = filter nontrivial contents
  pure contents'
  where
    nontrivial :: String -> Bool
    nontrivial = \case
      [] -> False
      c:_ -> c /= '.'


work :: FilePath -> IO ()
work file = do
  let inPath = "work/in/" <> file
  let outPath = "work/out/" <> file
  let debugPath = "work/out/" <> file <> ".tokens"
  putStrLn ("Parsing '" <> inPath <> "'.")
  result <- tokenize inPath
  case result of
    Left err -> Text.writeFile debugPath (Text.pack (errorBundlePretty err))
    Right stream -> do
      -- Text.writeFile debugPath (dumpTokens stream)
      result' <- parse document inPath stream
      case result' of
        Left err -> Text.writeFile outPath (Text.pack (errorBundlePretty err))
        Right doc -> withFile outPath WriteMode (\h -> hPutDoc h (prettyDocument doc))

dumpTokens :: TokStream -> Text
dumpTokens = Text.pack . show . fmap unLocated . unTokStream

tokenize :: FilePath -> IO (Either (ParseErrorBundle Text Void) TokStream)
tokenize path = do
  raw <- Text.readFile path
  let result = runParser toks path raw
  case result of
    Left err -> pure (Left err)
    Right stream -> pure (Right (TokStream raw stream))


parse :: Parser a -> String -> TokStream -> IO (Either (ParseErrorBundle TokStream Void) a)
parse p path stream = do
  let (result, _finalState) = runState (runParserT p path stream) initRegistry
  pure result
