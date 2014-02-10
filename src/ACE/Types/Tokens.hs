{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Tokens for ACE.

module ACE.Types.Tokens where

import Data.Data (Data,Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A token
data Token
  = Word (Int,Int) !Text
  | QuotedString (Int,Int) !Text
  | Period (Int,Int)
  | Comma (Int,Int)
  | QuestionMark (Int,Int)
  deriving (Show,Eq,Data,Typeable,Generic)
