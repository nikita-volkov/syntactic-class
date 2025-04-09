module SyntacticClass.Formats.InFixedBinary
  ( InFixedBinary (..),
  )
where

import SyntacticClass.Core
import qualified SyntacticClass.Formats.InFixedBinary.Attoparsec as Attoparsec
import SyntacticClass.Prelude
import qualified TextBuilder

-- | Wrapper for a value associating it with the fixed-width binary notation.
--
-- Provides instances for all types that are instances of 'FiniteBits' and 'Num',
-- which includes all ints and words.
newtype InFixedBinary a = InFixedBinary a
  deriving newtype (Eq, Ord, Arbitrary)
  deriving stock (Functor)

instance (FiniteBits a, Num a) => Syntactic (InFixedBinary a) where
  toTextBuilder (InFixedBinary base) = TextBuilder.binary base
  attoparsecParserOf = fmap InFixedBinary Attoparsec.dynamicWidthDigits

instance (FiniteBits a, Num a) => IsString (InFixedBinary a) where
  fromString = fromMaybe (InFixedBinary 0) . maybeFromText . fromString

instance (FiniteBits a, Num a) => Show (InFixedBinary a) where
  showsPrec d = showsPrec d . toText
