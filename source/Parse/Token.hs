{-# LANGUAGE RecordWildCards   #-}


module Parse.Token (module Parse.Token, module Export) where


{-
  Follows this section (by the main author of megaparsec):
  https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
-}


import Tokenize as Export (Tok(..), Delim(..), printTok, Located(..))

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Text.Megaparsec

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | A token stream for as input stream for a parser. Contains the raw input
-- before tokenization as @Text@ for showing error messages.
data TokStream = TokStream
  { rawInput :: Text
  , unTokStream :: [Located Tok]
  } deriving (Show, Eq)


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
      in case nonEmpty consumed of
        Nothing -> Just (consumed, TokStream raw ts')
        Just consumed' -> Just (consumed, TokStream (Text.drop (tokensLength pxy consumed') raw) ts')

  takeWhile_ :: (Token TokStream -> Bool) -> TokStream -> (Tokens TokStream, TokStream)
  takeWhile_ f (TokStream raw s) =
    let (x, s') = List.span f s
    in case nonEmpty x of
      Nothing -> (x, TokStream raw s')
      Just ts -> (x, TokStream (Text.drop (tokensLength pxy ts) raw) s')

  showTokens :: Proxy TokStream -> NonEmpty (Token TokStream) -> String
  showTokens Proxy ts = Text.unpack $ Text.intercalate " " $ NonEmpty.toList $ printTok <$> unLocated <$> ts

  tokensLength :: Proxy TokStream -> NonEmpty (Token TokStream) -> Int
  tokensLength Proxy xs = sum (tokenLength <$> xs)

  reachOffset :: Int -> PosState TokStream -> (String, PosState TokStream)
  reachOffset offset PosState {..} =
    ( Text.unpack prefix <> Text.unpack restOfLine
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
      prefix :: Text
      prefix = case sourceLine newSourcePos == sourceLine pstateSourcePos of
        True -> Text.pack pstateLinePrefix <> preRaw
        False -> preRaw
      newSourcePos = case post of
        [] -> pstateSourcePos -- Leave the source position untouched.
        (t:_) -> startPos t
      (pre, post) = splitAt (offset - pstateOffset) (unTokStream pstateInput)
      (preRaw, postRaw) = Text.splitAt tokensConsumed (rawInput pstateInput)
      tokensConsumed = case nonEmpty pre of
        Nothing -> 0
        Just nePre -> tokensLength pxy nePre
      restOfLine = Text.takeWhile (/= '\n') postRaw

pxy :: Proxy TokStream
pxy = Proxy

-- | Parses only the specified token. Note the polymorphic type.
-- We do not want to depend on or import any of the particularities
-- of the main parser (such as state) at the moment.
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

-- | @word@ parses a single word token. Case-insensitive.
word :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
word w = exactly (Word (Text.toCaseFold w))
{-# INLINE word #-}

anyWord :: (MonadParsec e s m, Token s ~ Located Tok) => m Text
anyWord = label "any word" $ token matcher Set.empty
  where
    -- We do not use @satisfy isWord@ for the implementation, since this
    -- yields an unnecessary @MonadFail@ constraint. This way we know enough
    -- to have a simpler type signature and we also have fine control over
    -- the error behavior.
    matcher :: Token TokStream -> Maybe Text
    matcher (Located _start _end _length t) = case t of
        Word w -> Just w
        _ -> Nothing
{-# INLINE anyWord #-}

-- | @word@ parses a single symbol token. Case-sensitive.
symbol :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
symbol s = exactly (Symbol s)
{-# INLINE symbol #-}

-- | @command@ parses a single command token. Case-sensitive.
command :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
command cmd = exactly (Command cmd)
{-# INLINE command #-}

-- | @variable@ parses any variable token. Case-sensitive.
variable :: (MonadParsec e s m, Token s ~ Located Tok) => m Text
variable = label "variable" $ token matcher Set.empty
  where
    -- We do not use @satisfy isVariable@ for the implementation, since this
    -- yields an unnecessary @MonadFail@ constraint. This way we know enough
    -- to have a simpler type signature and we also have fine control over
    -- the error behavior.
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
  return content
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
sepByComma1 :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p [a]
sepByComma1 p = p `sepBy1` (comma *> optional (word "and"))
{-# INLINE sepByComma1 #-}

comma :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
comma = void (symbol ",")
{-# INLINE comma #-}

period :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
period = void (symbol ".")
{-# INLINE period #-}
