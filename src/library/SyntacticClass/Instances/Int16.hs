{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Instances.Int16 where

import qualified Data.Attoparsec.Text as Attoparsec
import SyntacticClass.Core
import SyntacticClass.Prelude
import qualified TextBuilder

-- | Decimal notation.
instance Syntactic Int16 where
  toTextBuilder = TextBuilder.decimal
  attoparsecParserOf = Attoparsec.signed Attoparsec.decimal
