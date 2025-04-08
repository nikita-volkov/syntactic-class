{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Formats.InFixedBinary.Instances.Word8 where

import SyntacticClass.Class
import qualified SyntacticClass.Formats.InFixedBinary.Attoparsec as Attoparsec
import SyntacticClass.Formats.InFixedBinary.Core
import SyntacticClass.Prelude
import qualified TextBuilder

instance Syntactic (InFixedBinary Word8) where
  toTextBuilder (InFixedBinary base) = TextBuilder.binary base
  attoparsecParserOf = fmap InFixedBinary Attoparsec.dynamicWidthDigits

instance IsString (InFixedBinary Word8) where
  fromString = fromMaybe (InFixedBinary 0) . maybeFromText . fromString

instance Show (InFixedBinary Word8) where
  showsPrec d = showsPrec d . toText
