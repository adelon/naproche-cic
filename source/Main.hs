{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module Main where


import Base.Parser
import Base.Registry (initRegistry)
import Parse.Document

import Data.Void
import System.Directory
import Data.List ((\\))
import Control.Monad.State.Strict
import Data.Text (Text)

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
  result <- parseFromFile document inPath
  case result of
    Left err -> Text.writeFile outPath (Text.pack (show err))
    Right doc -> Text.writeFile outPath (process doc)


process :: _ -> Text
process = pack . show


parseFromFile
  :: Parser a
  -> FilePath
  -> IO (Either (ParseErrorBundle Text Void) a)
parseFromFile p path = do
  raw <- Text.readFile path
  let (result, _finalState) = runState (runParserT p path raw) initRegistry
  return result