{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Tokens for ACE.

module ACE.Types.Tokens where

import Data.Data (Data,Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A token
data Token
  = Word !Text
  | QuotedString !Text
  | Period
  | Comma
  | QuestionMark
  deriving (Show,Eq,Data,Typeable,Generic)
