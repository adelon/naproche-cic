{-# LANGUAGE RecordWildCards   #-}

module Tokenize where

{-
  This module defines the tokenizer, which takes TEX input in the form of Text
  and turn is into a list of tokens for further parsing.

  Follows this section (by the main author of megaparsec):
  https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
-}

import Data.Char
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Proxy
import Data.Text (Text)
import Data.Void
import Prelude hiding (Word)
import Text.Megaparsec


import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Megaparsec.Char as Lex


type Tokenizer = Parsec Void Text

data Tok
  = Word Text
  | Variable Text
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
  Variable v -> v
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
    TokStream raw ts ->
      let (consumed, ts') = splitAt n ts
      in case nonEmpty consumed of
        Nothing -> Just (consumed, TokStream raw ts')
        Just toksConsumed -> Just (consumed, TokStream (Text.drop (tokensLength pxy toksConsumed) raw) ts')

  takeWhile_ :: (Token TokStream -> Bool) -> TokStream -> (Tokens TokStream, TokStream)
  takeWhile_ f (TokStream raw s) =
    let (x, s') = List.span f s
    in case nonEmpty x of
      Nothing -> (x, TokStream raw s')
      Just nex -> (x, TokStream (Text.drop (tokensLength pxy nex) raw) s')

  showTokens :: Proxy TokStream -> NonEmpty (Token TokStream) -> String
  showTokens Proxy ts = Text.unpack $ Text.intercalate " " $ NonEmpty.toList $ printTok <$> tokenVal <$> ts

  tokensLength :: Proxy TokStream -> NonEmpty (Token TokStream) -> Int
  tokensLength Proxy xs = sum (tokenLength <$> xs)

  reachOffset :: Int -> PosState TokStream -> (String, PosState TokStream)
  reachOffset o PosState {..} =
    ( Text.unpack prefix <> Text.unpack restOfLine
    , PosState
        { pstateInput = TokStream
            { rawInput = postStr
            , unTokStream = post
            }
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = Text.unpack prefix
        }
    )
    where
      prefix :: Text
      prefix =
        if sameLine
          then Text.pack pstateLinePrefix <> preStr
          else preStr
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x:_) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unTokStream pstateInput)
      (preStr, postStr) = Text.splitAt tokensConsumed (rawInput pstateInput)
      tokensConsumed =
        case nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = Text.takeWhile (/= '\n') postStr

-- | Parses tokens, switching tokenizing mode when encountering math environments.
toks :: Tokenizer [Tok]
toks = go id
  where
    -- Instead of adding explicit state to our tokenizer we implement a token parser using two
    -- mutually recursive helper functions.
    go f = do
      r <- optional tok
      case r of
        Nothing -> return (f [])
        Just t@(BeginEnv "math") -> go' (f . (t:))
        Just t -> go (f . (t:))
    go' f = do
      r <- optional mathTok
      case r of
        Nothing -> return (f [])
        Just t@(EndEnv "math") -> go (f . (t:))
        Just t -> go' (f . (t:))
{-# INLINE toks #-}

-- | Parses a single normal mode token.
tok :: Tokenizer Tok
tok = word <|> symbol <|> begin <|> end <|> open <|> close <|> command

-- | Parses a single math mode token.
mathTok :: Tokenizer Tok
mathTok = var <|> symbol <|> begin <|> end <|> open <|> close <|> command

-- | Parses a word. Words are returned casefolded, since we want to ignore their case later on.
word :: Tokenizer Tok
word = lexeme do
  w <- some Lex.letterChar
  let w' = Text.toCaseFold (Text.pack w)
  return (Word w')

var :: Tokenizer Tok
var = Variable <$> lexeme (letter <|> bb <|> greek)
  where
    letter :: Tokenizer Text
    letter = Text.singleton <$> Lex.letterChar

    greek :: Tokenizer Text
    greek = do
      Lex.char '\\'
      l <- asum (makeSymbolParser <$> greeks)
      return l

    greeks :: [(Text,Text)]
    greeks =
      [ ("alpha", "α")
      , ("beta", "β")
      , ("gamma", "γ")
      , ("delta", "δ")
      -- ...
      , ("Gamma", "Γ")
      , ("Delta", "Δ")
      ]

    bb :: Tokenizer Text
    bb = do
      try (Lex.string "\\mathbb{")
      l <- asum (makeSymbolParser <$> bbs)
      Lex.char '}'
      return l

    bbs :: [(Text,Text)]
    bbs =
      [ ("A", "𝔸")
      , ("B", "𝔹")
      , ("C", "ℂ")
      , ("N", "ℕ")
      , ("P", "ℙ")
      , ("Q", "ℚ")
      , ("R", "ℝ")
      , ("Z", "ℤ")
      ]

    makeSymbolParser :: (Text, b) -> Tokenizer b
    makeSymbolParser (cmd, symb) = do
      Lex.string cmd
      return symb

symbol :: Tokenizer Tok
symbol = lexeme do
  symb <- some (satisfy (\c -> isDigit c || c `elem` symbols))
  return (Symbol (Text.pack symb))
    where
      symbols :: [Char]
      symbols = ".,:;!?@"

-- | Parses a TEX-style command.
command :: Tokenizer Tok
command = lexeme do
  Lex.char '\\'
  cmd <- some Lex.letterChar
  return (Command (Text.pack cmd))

-- | Parses the beginning of an environment. Commits only after having seen "\begin{".
begin :: Tokenizer Tok
begin = lexeme do
  try (Lex.string "\\begin{")
  env <- some Lex.letterChar
  Lex.char '}'
  return (BeginEnv (Text.pack env))

-- | Parses the end of an environment. Commits only after having seen "\end{".
end :: Tokenizer Tok
end = lexeme do
  try (Lex.string "\\end{")
  env <- some Lex.letterChar
  Lex.char '}'
  return (EndEnv (Text.pack env))

-- | Parses an opening delimiter.
open :: Tokenizer Tok
open = lexeme (paren <|> brace)
  where
    brace = Open Brace <$ lexeme (try (Lex.string "\\{"))
    paren = Open Paren <$ lexeme (Lex.char '(')

-- | Parses a closing delimiter.
close :: Tokenizer Tok
close = lexeme (paren <|> brace)
  where
    brace = Open Brace <$ lexeme (try (Lex.string "\\}"))
    paren = Open Paren <$ lexeme (Lex.char ')')

-- | Turns a tokenizer into one that consumes trailing whitespace.
lexeme :: Tokenizer a -> Tokenizer a
lexeme p = p <* Lex.space