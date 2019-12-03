module Tokenize (Tok(..), Located(..), TokStream(..), printTok, toks) where

{-
  This module defines the tokenizer, which takes TEX input in the form of Text
  and turn is into a list of tokens for further parsing.

  Follows this section (by the main author of megaparsec):
  https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
-}


import Prelude hiding (Word)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Proxy
import Data.Void
import Text.Megaparsec
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as Lex


type Tokenizer = Parsec Void Text

data Tok
  = Word Text
  | Symbol Text
  | Command Text
  | BeginEnv Text
  | EndEnv Text
  | Open Delim
  | Close Delim
  deriving (Show, Eq, Ord)

-- | Invisible delimiters are plain braces in TEX, braces are escaped braces.
data Delim = Invis | Paren | Brace | Bracket deriving (Show, Eq, Ord)

printTok :: Tok -> Text
printTok = \case
  Word w -> w
  Symbol s -> s
  Command cmd -> Text.cons '\\' cmd
  BeginEnv env -> "\\begin{" <> env <> "}"
  EndEnv env -> "\\end{" <> env <> "}"
  Open delim -> case delim of
    Invis -> "{"
    Paren -> "("
    Brace -> "\\{"
    Bracket -> "["
  Close delim -> case delim of
    Invis -> "}"
    Paren -> ")"
    Brace -> "\\}"
    Bracket -> "]"





data Located a = Located
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Show, Eq, Ord)

data TokStream = TokStream
  { rawInput :: Text -- for showing lines in error messages
  , unTokStream :: [Located Tok]
  }

pxy :: Proxy TokStream
pxy = Proxy

instance Stream TokStream where

  type Token TokStream = Located Tok
  type Tokens TokStream = [Located Tok]

  tokenToChunk :: Proxy TokStream -> Token TokStream -> Tokens TokStream
  tokenToChunk Proxy x = [x]

  tokensToChunk :: Proxy TokStream -> [Token TokStream] -> Tokens TokStream
  tokensToChunk Proxy xs = xs

  chunkToTokens :: Proxy TokStream -> Tokens TokStream -> [Token TokStream]
  chunkToTokens Proxy = id

  chunkLength :: Proxy TokStream -> Tokens TokStream -> Int
  chunkLength Proxy = length

  chunkEmpty :: Proxy TokStream -> Tokens TokStream -> Bool
  chunkEmpty Proxy = null

  take1_ :: TokStream -> Maybe (Token TokStream, TokStream)
  take1_ = \case
    TokStream _ [] -> Nothing
    TokStream raw (t:ts) ->
      let ts' = TokStream (Text.drop (tokenLength t) raw) ts
      in  Just (t,ts')

  takeN_ :: Int -> TokStream -> Maybe (Tokens TokStream, TokStream)
  takeN_ n | n <= 0 = \stream -> Just ([], stream)
  takeN_ n = \case
    TokStream _ [] -> Nothing
    TokStream raw toks ->
      let (consumed, toks') = splitAt n toks
      in case nonEmpty consumed of
        Nothing -> Just (consumed, TokStream raw toks')
        Just toksConsumed -> Just (consumed, TokStream (Text.drop (tokensLength pxy toksConsumed) raw) toks')


  takeWhile_ :: (Token TokStream -> Bool) -> TokStream -> (Tokens TokStream, TokStream)
  takeWhile_ = undefined

  showTokens :: Proxy TokStream -> NonEmpty (Token TokStream) -> String
  showTokens = undefined
  -- ...

toks :: Tokenizer [Tok]
toks = some tok

tok :: Tokenizer Tok
tok = word <|> command <|> begin <|> end <|> open <|> close

word :: Tokenizer Tok
word = do
  w <- some Lex.letterChar
  Lex.space
  return (Word (Text.pack w))

command :: Tokenizer Tok
command = do
  Lex.char '\\'
  cmd <- some Lex.letterChar
  Lex.space
  return (Command (Text.pack cmd))

begin :: Tokenizer Tok
begin = do
  return (BeginEnv undefined)

end :: Tokenizer Tok
end = do
  return (EndEnv undefined)

open :: Tokenizer Tok
open = do
  return (Open undefined)

close :: Tokenizer Tok
close = do
  return (Close undefined)