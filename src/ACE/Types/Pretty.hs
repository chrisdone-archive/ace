-- | Pretty printing types and classes.

module ACE.Types.Pretty where

import Data.Default

-- | Pretty print a syntax tree node to a string.
class Pretty p where
  pretty :: PrettySettings -> p -> String

instance Pretty a => Pretty (Maybe a) where
  pretty s = maybe "" (pretty s)

data PrettySettings = PrettySettings
  { prettyShowPrecedence :: Bool }

instance Default PrettySettings where
  def = PrettySettings { prettyShowPrecedence = True }
