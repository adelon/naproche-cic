module Tokenize where


import Prelude hiding (Word)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Void
import Text.Megaparsec
import Data.Text (Text)

import qualified Data.Text as Text


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
  type Token  TokStream = Located Tok
  type Tokens TokStream = [Located Tok]

  tokenToChunk Proxy x = [x]

  tokensToChunk Proxy xs = xs

  chunkToTokens Proxy = id

  chunkLength Proxy = length

  chunkEmpty Proxy = null

  take1_ :: TokStream -> Maybe (Token TokStream, TokStream)
  take1_ = \case
    TokStream _ [] -> Nothing
    TokStream raw (t:ts) ->
      let ts' = TokStream (Text.drop (tokenLength t) raw) ts
      in  Just (t,ts')

  -- ...