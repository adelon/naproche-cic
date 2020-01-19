module Tokenize where

{-
  This module defines the tokenizer, which takes TEX input in the form of Text
  and turn is into a list of tokens for further parsing.
-}

import Text.Megaparsec

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as Lex


type Tokenizer = Parsec Void Text

data Tok
  = Word Text
  | Variable Text
  | Symbol Text
  | Number Text
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
  Number n -> n
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
  , unLocated :: a
  } deriving (Show, Eq, Ord)

-- | Parses tokens, switching tokenizing mode when encountering math environments.
toks :: Tokenizer [Located Tok]
toks = go id
  where
    -- Instead of adding explicit state to our tokenizer we implement a token parser using two
    -- mutually recursive helper functions.
    go f = do
      r <- optional tok
      case r of
        Nothing -> return (f [])
        Just t@(Located _ _ _ (BeginEnv "math")) -> go' (f . (t:))
        Just t -> go (f . (t:))
    go' f = do
      r <- optional mathTok
      case r of
        Nothing -> return (f [])
        Just t@(Located _ _ _ (EndEnv "math")) -> go (f . (t:))
        Just t -> go' (f . (t:))
{-# INLINE toks #-}

-- | Parses a single normal mode token.
tok :: Tokenizer (Located Tok)
tok = word <|> symbol <|> begin <|> end <|> open <|> close <|> command <|> mathBegin

-- | Parses a single math mode token.
mathTok :: Tokenizer (Located Tok)
mathTok = var <|> symbol <|> number <|> begin <|> end <|> open <|> close <|> command <|> mathEnd

-- | Parses a single begin math token.
mathBegin :: Tokenizer (Located Tok)
mathBegin = lexeme do
  try (Lex.string "\\(" <|> Lex.string "\\[" <|> Lex.string "$")
  return (BeginEnv "math")

-- | Parses a single end math token.
mathEnd :: Tokenizer (Located Tok)
mathEnd = lexeme do
  try (Lex.string "\\)" <|> Lex.string "\\]" <|> Lex.string "$")
  return (EndEnv "math")

-- | Parses a word. Words are returned casefolded, since we want to ignore their case later on.
word :: Tokenizer (Located Tok)
word = lexeme do
  w <- some Lex.letterChar
  let t = Word (Text.toCaseFold (Text.pack w))
  return t

number :: Tokenizer (Located Tok)
number = lexeme do
  n <- some Lex.digitChar
  let t = Number (Text.pack n)
  return t

var :: Tokenizer (Located Tok)
var = lexeme $ Variable <$> (letter <|> bb <|> greek)
  where
    letter :: Tokenizer Text
    letter = Text.singleton <$> Lex.letterChar

    greek :: Tokenizer Text
    greek = try do
      Lex.char '\\'
      l <- asum (makeSymbolParser <$> greeks)
      notFollowedBy Lex.letterChar
      return l

    greeks :: [(Text,Text)]
    greeks =
      [ ("alpha", "α"), ("beta", "β"), ("gamma", "γ")
      , ("delta", "δ"), ("epsilon", "ε"), ("zeta", "ζ")
      , ("eta", "η"), ("theta", "θ"), ("iota", "ι")
      , ("kappa", "κ"), ("lambda", "λ"), ("mu", "μ")
      , ("nu", "ν"), ("xi", "ξ"), ("pi", "π")
      , ("rho", "ρ"), ("sigma", "σ"), ("tau", "τ")
      , ("upsilon", "υ"), ("phi", "φ"), ("chi", "χ")
      , ("psi", "ψ"), ("omega", "ω")
      , ("Gamma", "Γ"), ("Delta", "Δ"), ("Theta", "Θ")
      , ("Lambda", "Λ"), ("Xi", "Ξ"), ("Pi", "Π")
      , ("Sigma", "Σ"), ("Upsilon", "Υ"), ("Phi", "Φ")
      , ("Psi", "Ψ"), ("Omega", "Ω")
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

symbol :: Tokenizer (Located Tok)
symbol = lexeme do
  symb <- some (satisfy (`elem` symbols))
  return (Symbol (Text.pack symb))
    where
      symbols :: [Char]
      symbols = ".,:;!?@=+-/^><*"

-- | Parses a TEX-style command.
command :: Tokenizer (Located Tok)
command = lexeme $ try do
  Lex.char '\\'
  cmd <- some Lex.letterChar
  return (Command (Text.pack cmd))

-- | Parses the beginning of an environment. Commits only after having seen "\begin{".
begin :: Tokenizer (Located Tok)
begin = lexeme do
  try (Lex.string "\\begin{")
  env <- some Lex.letterChar
  Lex.char '}'
  return (BeginEnv (Text.pack env))

-- | Parses the end of an environment. Commits only after having seen "\end{".
end :: Tokenizer (Located Tok)
end = lexeme do
  try (Lex.string "\\end{")
  env <- some Lex.letterChar
  Lex.char '}'
  return (EndEnv (Text.pack env))

-- | Parses an opening delimiter.
open :: Tokenizer (Located Tok)
open = lexeme (paren <|> brace)
  where
    brace = Open Brace <$ lexeme (try (Lex.string "\\{"))
    paren = Open Paren <$ lexeme (Lex.char '(')

-- | Parses a closing delimiter.
close :: Tokenizer (Located Tok)
close = lexeme (paren <|> brace)
  where
    brace = Open Brace <$ lexeme (try (Lex.string "\\}"))
    paren = Open Paren <$ lexeme (Lex.char ')')

-- | Turns a tokenizer into one that tracks the source position of the token
-- and consumes trailing whitespace.
lexeme :: forall a. Tokenizer a -> Tokenizer (Located a)
lexeme p = do
  start <- getSourcePos
  startOffset <- getOffset
  t <- p
  Lex.space
  stop <- getSourcePos
  stopOffset <- getOffset
  let l = stopOffset - startOffset
  return (Located start stop l t)
