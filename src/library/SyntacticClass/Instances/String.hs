{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Instances.String where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import SyntacticClass.Core
import SyntacticClass.Prelude
import qualified TextBuilder

instance Syntactic String where
  toTextBuilder = TextBuilder.string

  attoparsecParserOf = Text.unpack <$> Attoparsec.takeText
