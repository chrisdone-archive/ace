-- | Pretty printing types and classes.

module ACE.Types.Pretty where

import Data.Default

-- | Pretty print a syntax tree node to a string.
class Pretty p where
  pretty :: PrettySettings -> p -> String

-- | Prints no string if nothing.
instance Pretty a => Pretty (Maybe a) where
  pretty s = maybe "" (pretty s)

-- | Settings used for pretty printing.
data PrettySettings = PrettySettings
  { prettyShowPrecedence :: Bool -- ^ Show precedence?
  }

-- | Precedence showing enabled by default.
instance Default PrettySettings where
  def = PrettySettings { prettyShowPrecedence = True }
