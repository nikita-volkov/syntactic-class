{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Formats.InFixedBinary.Instances.Int where

import SyntacticClass.Class
import qualified SyntacticClass.Formats.InFixedBinary.Attoparsec as Attoparsec
import SyntacticClass.Formats.InFixedBinary.Core
import SyntacticClass.Prelude
import qualified TextBuilder

instance Syntactic (InFixedBinary Int) where
  toTextBuilder (InFixedBinary base) = TextBuilder.binary base
  attoparsecParserOf = fmap (InFixedBinary . fromIntegral) (Attoparsec.dynamicWidthDigits @Word)

instance IsString (InFixedBinary Int) where
  fromString = fromMaybe (InFixedBinary 0) . maybeFromText . fromString

instance Show (InFixedBinary Int) where
  showsPrec d = showsPrec d . toText
