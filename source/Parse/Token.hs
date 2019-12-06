{-# LANGUAGE RecordWildCards   #-}


module Parse.Token where


{-
  Follows this section (by the main author of megaparsec):
  https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
-}


import Tokenize (Tok(..), Delim(..), printTok)

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Proxy
import Data.Text (Text)
import Data.Set (Set)
import Prelude hiding (Word)
import Text.Megaparsec

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text


data Located a = Located
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Show, Eq, Ord)

-- | A token stream for as input stream for a parser. Contains the raw input before tokenization
-- as @Text@ for showing error messages.
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

pxy :: Proxy TokStream
pxy = Proxy

-- | Parses only the specified token. Note the polymorphic type. We do not want to depend on
-- or import any of the particularities of the main parser (such as state) at the moment.
exactly :: (MonadParsec e s p, Token s ~ Located Tok) => Tok -> p Tok
exactly c = token matcher expectation
  where
    -- This set describes which items were expected. In this case it is just
    -- the single token @c@ that we lift into this set.
    expectation :: Set (ErrorItem (Located Tok))
    expectation = Set.singleton (Tokens (liftTok c :| []))

    -- Matching function for token parsing.
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

-- | @word@ parses a single symbol token.
symbol :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
symbol s = exactly (Symbol s)

-- | @command@ parses a single command token.
command :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
command cmd = exactly (Command cmd)

-- | @begin@ and @end@ each parse a single begin and end token.
begin, end :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p Tok
begin env = exactly (BeginEnv env)
end env = exactly (EndEnv env)

surroundedBy :: Applicative p => p t -> p a -> p b -> p b
surroundedBy open close p = do
  open
  content <- p
  close
  return content
{-# INLINE surroundedBy #-}

environment :: (MonadParsec e s p, Token s ~ Located Tok) => Text -> p a -> p a
environment env = surroundedBy (begin env) (end env)

math :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
math = environment "math"

delimited :: (MonadParsec e s p, Token s ~ Located Tok) => Delim -> p a -> p a
delimited delim = surroundedBy (exactly (Open delim)) (exactly (Close delim))
{-# INLINE delimited #-}

grouped :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
grouped = delimited Invis

braced :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
braced = delimited Brace

parenthesized :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
parenthesized = delimited Paren

bracketed :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p a
bracketed = delimited Bracket



-- | Parses a list separated by "," or ", and".
sepByComma :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p [a]
sepByComma p = p `sepBy` (comma *> optional (word "and"))

-- | Parses a list of at least one item separated by "," or ", and".
sepByComma1 :: (MonadParsec e s p, Token s ~ Located Tok) => p a -> p [a]
sepByComma1 p = p `sepBy1` (comma *> optional (word "and"))

comma :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
comma = void (symbol ",")

period :: (MonadParsec e s p, Token s ~ Located Tok) => p ()
period = void (symbol ".")