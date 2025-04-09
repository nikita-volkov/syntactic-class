{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Formats.InIso8601.Instances.Day where

import qualified Attoparsec.Data as AttoparsecData
import Data.Time
import SyntacticClass.Core
import qualified SyntacticClass.Formats.InIso8601.Builders as Builders
import SyntacticClass.Formats.InIso8601.Core
import SyntacticClass.Prelude

-- | Day in ISO-8601 format.
--
-- >>> toText (InIso8601 (fromGregorian 2021 11 24))
-- "2021-11-24"
--
-- >>> maybeFromText "2021-11-24" :: Maybe (InIso8601 Day)
-- Just "2021-11-24"
--
-- >>> toText (InIso8601 (fromGregorian 2000 12 31))
-- "2000-12-31"
instance Syntactic (InIso8601 Day) where
  toTextBuilder (InIso8601 base) = Builders.day base

  attoparsecParserOf = fmap InIso8601 AttoparsecData.dayInISO8601

-- | String literal in ISO-8601 format.
--
-- >>> fromString "2021-11-24" :: InIso8601 Day
-- "2021-11-24"
--
-- Invalid inputs get parsed as the Unix epoch.
--
-- >>> fromString "z" :: InIso8601 Day
-- "1970-01-01"
instance IsString (InIso8601 Day) where
  fromString = fromMaybe def . maybeFromText . fromString
    where
      def = InIso8601 (fromGregorian 1970 1 1)

-- | String literal in ISO-8601 format.
--
-- >>> show (InIso8601 (fromGregorian 2000 12 31))
-- "\"2000-12-31\""
instance Show (InIso8601 Day) where
  showsPrec d = showsPrec d . toText
