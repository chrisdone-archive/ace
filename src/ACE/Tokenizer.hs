-- | Tokenizer for ACE.

module ACE.Tokenizer where

import ACE.Types.Tokens

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Text

-- | Tokenize some complete ACE text.
tokenize :: Text -> Either String [Token]
tokenize =
  parseOnly (tokenizer <* endOfInput)

-- | The tokenizer.
tokenizer :: Parser [Token]
tokenizer =
  many (skipSpace *> token)

-- | Parse a token.
token :: Parser Token
token =
  quotedString <|>
  period <|>
  comma <|>
  questionMark <|>
  word

-- | Parse a quoted string, @\"foobar\"@.
quotedString :: Parser Token
quotedString =
  char '"' *> (QuotedString <$> takeWhile1 (/='"')) <* char '"'

-- | Parse a period \".\".
period :: Parser Token
period =
  char '.' *> pure Period

-- | Parse a comma \",\".
comma :: Parser Token
comma =
  char ',' *> pure Comma

-- | Parse a question mark \"?\".
questionMark :: Parser Token
questionMark =
  char '?' *> pure QuestionMark

-- | Parse a word, which is any sequence of non-whitespace words
-- containing none of the other token characters.
word :: Parser Token
word =
  Word <$> (takeWhile1 wordChar <* skipSpace)
  where
    wordChar c =
      not (isSpace c) &&
      c /= '"' &&
      c /= '.' &&
      c /= '?' &&
      c /= ','
