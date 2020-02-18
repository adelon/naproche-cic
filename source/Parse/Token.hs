{-# LANGUAGE RecordWildCards   #-}

-- Definition of the token stream, functions on that token stream and
-- basic token parsers.
--
-- This module reexports some basic functions from 'Tokenize' to enable using
-- this module without a second import (in most cases).

module Parse.Token (module Parse.Token, module Export) where


import Tokenize as Export (Tok(..), Delim(..), printTok, Located(..))

import Text.Megaparsec

import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text

-- |
-- A token stream for as input stream for a parser. Contains the raw input
-- before tokenization as @Text@ for showing error messages.
--
data TokStream = TokStream
   { rawInput :: Text
   , unTokStream :: [Located Tok]
   } deriving (Show, Eq)


-- The stream instance follows this guide (by the main maintainer of megaparsec):
-- https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams

instance Stream TokStream where

   type Token TokStream = Located Tok
   type Tokens TokStream = [Located Tok]

   tokenToChunk :: Proxy TokStream -> Token TokStream -> Tokens TokStream
   tokenToChunk Proxy x = [x]

   tokensToChunk :: Proxy TokStream -> [Token TokStream] -> Tokens TokStream
   tokensToChunk Proxy = id

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
         in case NonEmpty.nonEmpty consumed of
         Nothing -> Just (consumed, TokStream raw ts')
         Just consumed' -> Just (consumed, TokStream (Text.drop (tokensLength pxy consumed') raw) ts')

   takeWhile_ :: (Token TokStream -> Bool) -> TokStream -> (Tokens TokStream, TokStream)
   takeWhile_ f (TokStream raw s) =
      let (x, s') = List.span f s
      in case NonEmpty.nonEmpty x of
         Nothing -> (x, TokStream raw s')
         Just ts -> (x, TokStream (Text.drop (tokensLength pxy ts) raw) s')

   showTokens :: Proxy TokStream -> NonEmpty (Token TokStream) -> String
   showTokens Proxy ts =
      Text.unpack $ quote $ Text.intercalate " " $ ts'
         where
         ts' = NonEmpty.toList $ printTok <$> unLocated <$> ts
         quote t = "'" <> t <> "'"

   tokensLength :: Proxy TokStream -> NonEmpty (Token TokStream) -> Int
   tokensLength Proxy xs = sum (tokenLength <$> xs)

   reachOffset :: Int -> PosState TokStream -> (String, PosState TokStream)
   reachOffset offset PosState {..} =
      ( Text.unpack (lastLine prefix <> restOfLine)
      , PosState
         { pstateInput = TokStream
         { rawInput = postRaw
         , unTokStream = post
         }
         , pstateOffset = max pstateOffset offset
         , pstateSourcePos = newSourcePos
         , pstateTabWidth = pstateTabWidth
         , pstateLinePrefix = Text.unpack prefix
         }
      )
      where
      -- We need to strip everything but the last line of the prefix
      -- so that the error renders correctly and concisely. The naive
      -- implementation here should suffice, since this will only be
      -- called at most once (when reporting a parse error).
      lastLine :: Text -> Text
      lastLine cs = case Text.lines cs of
         [] -> ""
         lines -> List.last lines
      prefix :: Text
      prefix = case sourceLine newSourcePos == sourceLine pstateSourcePos of
         True -> Text.pack pstateLinePrefix <> preRaw
         False -> preRaw
      newSourcePos = case post of
         [] -> pstateSourcePos -- Leave the source position untouched.
         (t:_) -> startPos t
      (pre, post) = splitAt (offset - pstateOffset) (unTokStream pstateInput)
      (preRaw, postRaw) = Text.splitAt tokensConsumed (rawInput pstateInput)
      tokensConsumed = case NonEmpty.nonEmpty pre of
         Nothing -> 0
         Just nePre -> tokensLength pxy nePre
      restOfLine = Text.takeWhile (/= '\n') postRaw

pxy :: Proxy TokStream
pxy = Proxy

-- |
-- Parses only the specified token. Note the polymorphic type.
-- We do not want to depend on or import any of the particularities
-- of the main parser (such as state) at the moment.
--
exactly :: (MonadParsec e s p, Token s ~ Located Tok) => Tok -> p Tok
exactly c = token matcher expectation
   where
   expectation :: Set (ErrorItem (Located Tok))
   expectation = Set.singleton (Tokens (liftTok c :| []))

   matcher :: Token TokStream -> Maybe Tok
   matcher (Located _start _end _length t) =
      if t == c
      then Just t
      else Nothing

   liftTok :: Tok -> Located Tok
   liftTok t = Located pos pos 0 t
      where
      pos :: SourcePos
      pos = initialPos ""

anyTokenBut :: (MonadParsec e s m, Token s ~ Located Tok) => Set Tok -> m Tok
anyTokenBut toks = token matcher Set.empty
   where
   matcher :: Token TokStream -> Maybe Tok
   matcher (Located _start _end _length tok) =
      if tok `Set.member` toks then Nothing else Just tok
{-# INLINE anyTokenBut #-}

-- | @word@ parses a single word token. Case-insensitive.
word :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
word w = exactly (Word (Text.toCaseFold w))
{-# INLINE word #-}

-- | @word@ parses a specified list of word tokens, discarding the result. Case-insensitive.
words :: (MonadParsec e s p, Token s ~ Located Tok) => [Text] -> p ()
words ws = for_ ws word
{-# INLINE words #-}

anyWord :: (MonadParsec e s m, Token s ~ Located Tok) => m Text
anyWord = label "any word" $ token matcher Set.empty
   where
   matcher :: Token TokStream -> Maybe Text
   matcher (Located _start _end _length t) = case t of
      Word w -> Just w
      _ -> Nothing
{-# INLINE anyWord #-}

anyWordBut :: (MonadParsec e s m, Token s ~ Located Tok) => Set Text -> m Text
anyWordBut ws = token matcher Set.empty
   where
   matcher :: Token TokStream -> Maybe Text
   matcher (Located _start _end _length t) = case t of
      Word w ->
         if w `Set.member` ws
            then Nothing
            else Just w
      _ -> Nothing
{-# INLINE anyWordBut #-}

-- | @word@ parses a single symbol token. Case-sensitive.
symbol :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
symbol s = exactly (Symbol s)
{-# INLINE symbol #-}

anySymbol :: (MonadParsec e s m, Token s ~ Located Tok) => m Text
anySymbol = label "any word" $ token matcher Set.empty
   where
   matcher :: Token TokStream -> Maybe Text
   matcher (Located _start _end _length t) = case t of
      Symbol symb -> Just symb
      _ -> Nothing
{-# INLINE anySymbol #-}

-- TODO: This should probably return an actual number instead of Text.
anyNumber :: (MonadParsec e s m, Token s ~ Located Tok) => m Text
anyNumber = label "any number" $ token matcher Set.empty
   where
   matcher :: Token TokStream -> Maybe Text
   matcher (Located _start _end _length t) = case t of
      Number n -> Just n
      _ -> Nothing
{-# INLINE anyNumber #-}

-- | @command@ parses a single command token. Case-sensitive.
command :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
command cmd = exactly (Command cmd)
{-# INLINE command #-}

anyCommand :: (MonadParsec e s m, Token s ~ Located Tok) => m Text
anyCommand = label "any word" $ token matcher Set.empty
   where
   matcher :: Token TokStream -> Maybe Text
   matcher (Located _start _end _length t) = case t of
      Command cmd -> Just cmd
      _ -> Nothing
{-# INLINE anyCommand #-}

-- | @variable@ parses any variable token. Case-sensitive.
variable :: (MonadParsec e s m, Token s ~ Located Tok) => m Text
variable = label "variable" $ token matcher Set.empty
   where
   matcher :: Token TokStream -> Maybe Text
   matcher (Located _start _end _length t) = case t of
      Variable v -> Just v
      _ -> Nothing
{-# INLINE variable #-}

begin :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
begin env = exactly (BeginEnv env)
{-# INLINE begin #-}

end :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
end env = exactly (EndEnv env)
{-# INLINE end #-}

surroundedBy :: Applicative p => p t -> p a -> p b -> p b
surroundedBy start stop p = do
   start
   content <- p
   stop
   pure content
{-# INLINE surroundedBy #-}

environment :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p a -> p a
environment env = surroundedBy (begin env) (end env)
{-# INLINE environment #-}

math :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
math = environment "math"
{-# INLINE math #-}

open :: (MonadParsec e s p, Token s ~ Located Tok) => Delim -> p Tok
open delim = exactly (Open delim)
{-# INLINE open #-}

close :: (MonadParsec e s p, Token s ~ Located Tok) => Delim -> p Tok
close delim = exactly (Close delim)
{-# INLINE close #-}

delimited :: (MonadParsec e s p, Token s ~ Located Tok) => Delim -> p a -> p a
delimited delim = surroundedBy (open delim) (close delim)
{-# INLINE delimited #-}

grouped :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
grouped = delimited Invis
{-# INLINE grouped #-}

braced :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
braced = delimited Brace
{-# INLINE braced #-}

parenthesized :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
parenthesized = delimited Paren
{-# INLINE parenthesized #-}

bracketed :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
bracketed = delimited Bracket
{-# INLINE bracketed #-}

-- | Parses a list separated by "," or ", and".
sepByComma :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p [a]
sepByComma p = p `sepBy` (comma *> optional (word "and"))
{-# INLINE sepByComma #-}

-- | Parses a list of at least one item separated by "," or ", and".
sepByComma1 :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p (NonEmpty a)
sepByComma1 p = p `NonEmpty.sepBy1` (comma *> optional (word "and"))
{-# INLINE sepByComma1 #-}

comma :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
comma = void (symbol ",")
{-# INLINE comma #-}

period :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
period = void (symbol ".")
{-# INLINE period #-}

-- | 
-- Parses the word 'iff', 'whenever' or the phrase 'if and only if'.
-- Commits to the latter phrase after parsing 'only'. Always returns 'iff' on
-- success, so that we can easily pattern match on the result.
iff :: (MonadParsec e s p, Token s ~ Located Tok) => p Tok
iff = Word "iff" <$ do
   word "whenever" <|> word "iff"
   <|> (try (words ["if", "and", "only"]) *> word "if")
{-# INLINE iff #-}

-- |
-- A laxer version of `iff`, which also accepts 'if'.
-- Intended for use in defining phrases.
iff' :: (MonadParsec e s p, Token s ~ Located Tok) => p Tok
iff' = Word "iff" <$ do
   word "whenever" <|> word "iff"
   <|> (word "if" <* optional (words ["and", "only", "if"]))
{-# INLINE iff' #-}

-- An informal survey of mathematical texts (and English texts in general)
-- showed that we may commit to an existential on the word 'there' already.
-- Thus `thereExists` does not backtrack. We also allow a copula with an existential
-- meaning, as in 'there is a natural number n suh that ...'.
--
thereExists :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
thereExists = void $ word "there" *> (word "exist" <|> word "exists" <|> copula)
{-# INLINE thereExists #-}

-- | Parses the phrase 'such that'. Backtracks.
suchThat :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
suchThat = void $ try $ word "such" *> word "that"
{-# INLINE suchThat #-}

weHave :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
weHave = void $ word "we" *> word "have" *> optional (word "that")

copula :: (MonadParsec e s p, Token s ~ Located Tok) => p Tok
copula = (word "is" <|> word "are") >> pure (Word "is")

copulaPlural :: (MonadParsec e s p, Token s ~ Located Tok) => Int -> p Tok
copulaPlural = plural (word "is") (word "are")

indefinite :: (MonadParsec e s p, Token s ~ Located Tok) => p Tok
indefinite = (word "a" <|> word "an") >> pure (Word "an")

definite :: (MonadParsec e s p, Token s ~ Located Tok) => p Tok
definite = word "the"
