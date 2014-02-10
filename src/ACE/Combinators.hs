{-# LANGUAGE FlexibleContexts #-}

-- | Parser combinators.

module ACE.Combinators where

import           ACE.Types.Tokens

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec.Pos
import           Text.Parsec.Prim

-- | Match a word with the given string.
string :: Stream s m Token => Text -> ParsecT s u m Token
string s =
  satisfy (\t -> case t of
                   Word _ t -> t == s)

-- | Match a Saxon genitive.
genitive :: Stream s m Token => Bool -> ParsecT s u m Token
genitive s =
  satisfy (\t -> case t of
                   Genitive _ hasS -> hasS == s)

-- | Try to match all the given strings, or none at all.
strings :: Stream s m Token => [Text] -> ParsecT s u m ()
strings ss = try (sequence_ (map string ss))

-- | Satisfy the given predicate from the token stream.
satisfy :: Stream s m Token => (Token -> Bool) -> ParsecT s u m Token
satisfy f =
  tokenPrim tokenString
            tokenPosition
            (\c -> if f c then Just c else Nothing)
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
