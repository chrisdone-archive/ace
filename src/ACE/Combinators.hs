{-# LANGUAGE FlexibleContexts #-}

-- | Parser combinators.

module ACE.Combinators where

import           ACE.Types.Tokens

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec.Pos
import           Text.Parsec.Prim

-- | Match a word with the given string.
string :: Stream s m Token => Text -> ParsecT s u m Text
string s =
  satisfy
    (\t ->
       case t of
         Word _ t -> if t == s then Just t else Nothing
         _ -> Nothing)

-- | Match a Saxon genitive.
genitive :: Stream s m Token => ParsecT s u m Bool
genitive =
  satisfy
    (\t ->
       case t of
         Genitive _ hasS -> Just hasS
         _ -> Nothing)

-- | Match a word with the given string.
number :: Stream s m Token => ParsecT s u m Integer
number =
  satisfy
    (\t ->
       case t of
         Number _ t -> Just t
         _ -> Nothing)

-- | Quoted string.
quoted :: Stream s m Token => ParsecT s u m Text
quoted =
  satisfy
    (\t ->
       case t of
         QuotedString _ t -> Just t
         _ -> Nothing)

-- | A comma.
comma :: Stream s m Token => ParsecT s u m ()
comma =
  satisfy
    (\t ->
       case t of
         Comma _ -> Just ()
         _ -> Nothing)

-- | A period.
period :: Stream s m Token => ParsecT s u m ()
period =
  satisfy
    (\t ->
       case t of
         Period _ -> Just ()
         _ -> Nothing)

-- | Try to match all the given strings, or none at all.
strings :: Stream s m Token => [Text] -> ParsecT s u m ()
strings ss =
  try (sequence_ (map string ss))

-- | Satisfy the given predicate from the token stream.
satisfy :: Stream s m Token => (Token -> Maybe a) -> ParsecT s u m a
satisfy f =
  tokenPrim tokenString
            tokenPosition
            f
  where tokenString t =
          case t of
            Word _ w -> "word \"" ++ T.unpack w ++ "\""
            QuotedString _ s -> "quotation \"" ++ T.unpack s ++ "\""
            Period{} -> "period"
            Comma{} -> "comma"
            QuestionMark{} -> "question mark"
            Genitive _ s ->
              if s
                 then "genitive 's"
                 else "genitive '"
        tokenPosition pos t _ =
          setSourceColumn (setSourceLine pos line) col
          where (line,col) = tokenPos t
