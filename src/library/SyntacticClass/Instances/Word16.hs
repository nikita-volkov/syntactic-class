{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Instances.Word16 where

import qualified Data.Attoparsec.Text as Attoparsec
import SyntacticClass.Core
import SyntacticClass.Prelude
import qualified TextBuilder

-- | Decimal notation.
instance Syntactic Word16 where
  toTextBuilder = TextBuilder.decimal
  attoparsecParserOf = Attoparsec.decimal
