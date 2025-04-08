module SyntacticClass.Formats.InFixedBinary.Core where

import SyntacticClass.Prelude

-- | Wrapper for a value associating it with the fixed-width binary notation.
newtype InFixedBinary a = InFixedBinary a
  deriving newtype (Eq, Ord, Arbitrary)
