{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Formats.InFixedBinary.Instances.Word where

import SyntacticClass.Class
import qualified SyntacticClass.Formats.InFixedBinary.Attoparsec as Attoparsec
import SyntacticClass.Formats.InFixedBinary.Core
import SyntacticClass.Prelude
import qualified TextBuilder

instance Syntactic (InFixedBinary Word) where
  toTextBuilder (InFixedBinary base) = TextBuilder.binary base
  attoparsecParserOf = fmap InFixedBinary Attoparsec.dynamicWidthDigits

instance IsString (InFixedBinary Word) where
  fromString = fromMaybe (InFixedBinary 0) . maybeFromText . fromString

instance Show (InFixedBinary Word) where
  showsPrec d = showsPrec d . toText
