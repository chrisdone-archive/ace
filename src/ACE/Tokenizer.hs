{-# LANGUAGE BangPatterns #-}
-- | Tokenizer for ACE. Tokens retain source locations (line and column).

module ACE.Tokenizer where

import           ACE.Types.Tokens

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Attoparsec.Text hiding (number)
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- | Tokenize some complete ACE text.
tokenize :: Text -> Either String [Token]
tokenize =
  parseOnly (fmap fst tokenizer <* endOfInput)

-- | The tokenizer.
tokenizer :: Parser ([Token],(Int,Int))
tokenizer =
  manyWithPos (spaces >=> token)
              genitive
              (1,0)

-- | Parse a token.
token :: (Int,Int) -> Parser (Token,(Int,Int))
token pos =
  number pos <|>
  quotedString pos <|>
  period pos <|>
  comma pos <|>
  questionMark pos <|>
  word pos

-- | Parse a number.
number :: (Int, Int) -> Parser (Token, (Int, Int))
number pos =
  fmap (\n -> (Number pos (either (const 0) fst (T.decimal n)),second (+ T.length n) pos))
       (takeWhile1 isDigit)

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
      c /= ',' &&
      c /= '\''

-- | Parse the Saxon genitive ' or 's. This is ran after parsing every
-- token, but is expected to fail most of the time.
genitive :: (Int, Int) -> Parser (Maybe (Token, (Int, Int)))
genitive pos =
  optional go
  where
    go =
      do char '\''
         ms <- peekChar
         case ms of
           Just 's' -> anyChar *> pure (Genitive pos True,second (+1) pos)
           _ -> pure (Genitive pos False,second (+1) pos)

-- | Like 'many', but retains the current source position and supports
-- postfix-parsing of the genitive apostrophe.
manyWithPos :: (Monad m, Alternative m)
            => ((t, t1) -> m (a, (t, t1))) -> ((t, t1) -> m (Maybe (a, (t, t1))))
            -> (t, t1) -> m ([a], (t, t1))
manyWithPos p p' pos =
  do r <- fmap (first Just) (p pos) <|> pure (Nothing,pos)
     case r of
       (Nothing,_) ->
         return ([],pos)
       (Just x,newpos@(!_,!_)) ->
         do r <- p' newpos
            case r of
              Nothing ->
                do (xs,finalpos) <- manyWithPos p p' newpos
                   return (x:xs,finalpos)
              Just (y,newpos') ->
                do (xs,finalpos) <- manyWithPos p p' newpos'
                   return (x:y:xs,finalpos)

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
