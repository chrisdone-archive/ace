{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Tokens for ACE.

module ACE.Types.Tokens
  (Token(..)
  ,tokenPos)
  where

import Data.Text (Text)

-- | A token
data Token
  = Word !(Int,Int) !Text
  | QuotedString !(Int,Int) !Text
  | Period !(Int,Int)
  | Comma !(Int,Int)
  | QuestionMark !(Int,Int)
  | Genitive !(Int,Int) !Bool
  | Number !(Int,Int) !Integer
  deriving (Eq)

-- | Get the position of the token.
tokenPos :: Token -> (Int, Int)
tokenPos t =
  case t of
    Word pos _         -> pos
    QuotedString pos _ -> pos
    Period pos         -> pos
    Comma pos          -> pos
    QuestionMark pos   -> pos
    Genitive pos _     -> pos
    Number pos _       -> pos
