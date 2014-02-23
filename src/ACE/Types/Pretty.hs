{-# LANGUAGE OverloadedStrings #-}

-- | Pretty printing types and classes.

module ACE.Types.Pretty where

import Data.Text.Lazy.Builder

-- | Pretty print a syntax tree node to a string.
class Pretty p where
  pretty :: p -> Builder

-- | Prints no string if nothing.
instance Pretty a => Pretty (Maybe a) where
  pretty = maybe "" pretty
