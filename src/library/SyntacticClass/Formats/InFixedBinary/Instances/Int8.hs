{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Formats.InFixedBinary.Instances.Int8 where

import SyntacticClass.Class
import qualified SyntacticClass.Formats.InFixedBinary.Attoparsec as Attoparsec
import SyntacticClass.Formats.InFixedBinary.Core
import SyntacticClass.Prelude
import qualified TextBuilder

instance Syntactic (InFixedBinary Int8) where
  toTextBuilder (InFixedBinary base) = TextBuilder.binary base
  attoparsecParserOf = fmap (InFixedBinary . fromIntegral) (Attoparsec.dynamicWidthDigits @Word8)

instance IsString (InFixedBinary Int8) where
  fromString = fromMaybe (InFixedBinary 0) . maybeFromText . fromString

instance Show (InFixedBinary Int8) where
  showsPrec d = showsPrec d . toText
