module SyntacticClass.Formats.InIso8601.Core where

import SyntacticClass.Prelude

-- | Wrapper for a value associating it with the ISO-8601 format.
newtype InIso8601 a = InIso8601 a
  deriving newtype (Eq, Ord, Arbitrary)
  deriving stock (Functor)
