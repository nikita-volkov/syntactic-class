{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Instances.LazyText where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text.Lazy as LazyText
import SyntacticClass.Class
import qualified TextBuilder

instance Syntactic LazyText.Text where
  toTextBuilder = TextBuilder.lazyText

  attoparsecParserOf = Attoparsec.takeLazyText
