{-# LANGUAGE BangPatterns #-}
-- | Tokenizer for ACE. Tokens retain source locations (line and column).

module ACE.Tokenizer where

import           ACE.Types.Tokens

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T

-- | Tokenize some complete ACE text.
tokenize :: Text -> Either String [Token]
tokenize =
  parseOnly (fmap fst tokenizer <* endOfInput)

-- | The tokenizer.
tokenizer :: Parser ([Token],(Int,Int))
tokenizer =
  manyWithPos (spaces >=> token) (1,0)

-- | Parse a token.
token :: (Int,Int) -> Parser (Token,(Int,Int))
token pos =
  quotedString pos <|>
  period pos <|>
  comma pos <|>
  questionMark pos <|>
  word pos

-- | Parse a quoted string, @\"foobar\"@.
quotedString :: (Int,Int) -> Parser (Token,(Int,Int))
quotedString pos =
  char '"' *> (cons <$> takeWhile1 (/='"')) <* char '"'
  where
    cons x =
      (QuotedString pos x
      ,second (+ (T.length x + 2)) pos)

-- | Parse a period \".\".
period :: (Int,Int) -> Parser (Token,(Int,Int))
period pos =
  char '.' *> pure (Period pos,second (+1) pos)

-- | Parse a comma \",\".
comma :: (Int,Int) -> Parser (Token,(Int,Int))
comma pos =
  char ',' *> pure (Comma pos,second (+1) pos)

-- | Parse a question mark \"?\".
questionMark :: (Int,Int) -> Parser (Token,(Int,Int))
questionMark pos =
  char '?' *> pure (QuestionMark pos,second (+1) pos)

-- | Parse a word, which is any sequence of non-whitespace words
-- containing none of the other token characters.
word :: (Int,Int) -> Parser (Token,(Int,Int))
word pos =
  cons <$> takeWhile1 wordChar
  where
    cons w = (Word pos w,second (+ T.length w) pos)
    wordChar c =
      not (isSpace c) &&
      c /= '"' &&
      c /= '.' &&
      c /= '?' &&
      c /= ','

-- | Like 'many', but retains the current source position.
manyWithPos :: (Monad m, Alternative m)
            => ((Int,Int) -> m (a, (Int,Int)))
            -> (Int,Int)
            -> m ([a], (Int,Int))
manyWithPos p pos =
  do r <- fmap (first Just) (p pos) <|> pure (Nothing,pos)
     case r of
       (Nothing,_) ->
         return ([],pos)
       (Just x,newpos@(!_,!_)) ->
         do (xs,finalpos) <- manyWithPos p newpos
            return (x:xs,finalpos)

-- | Skip spaces (space, newline, tab (=4 spaces)) and keep
-- positioning information up to date.
spaces :: (Int,Int) -> Parser (Int,Int)
spaces (sline,scol) =
  go sline scol
  where
    go line col =
      do c <- peekChar
         case c of
           Just '\n' -> anyChar *> go (line+1) 0
           Just ' ' -> anyChar *> go line (col+1)
           Just '\t' -> anyChar *> go line (col+4)
           _ -> return (line,col)
