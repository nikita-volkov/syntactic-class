{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Instances.Text where

import qualified Data.Attoparsec.Text as Attoparsec
import SyntacticClass.Core
import SyntacticClass.Prelude
import qualified TextBuilder

instance Syntactic Text where
  toTextBuilder = TextBuilder.text

  attoparsecParserOf = Attoparsec.takeText
