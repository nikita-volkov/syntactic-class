{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Instances.Uuid where

import qualified Attoparsec.Data as AttoparsecData
import qualified Data.UUID as Uuid
import SyntacticClass.Class
import SyntacticClass.Prelude
import qualified TextBuilder

instance Syntactic Uuid.UUID where
  toTextBuilder = TextBuilder.text . Uuid.toText

  attoparsecParserOf = AttoparsecData.uuid
