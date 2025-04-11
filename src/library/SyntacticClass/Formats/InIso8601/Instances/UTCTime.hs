{-# OPTIONS_GHC -Wno-orphans #-}

module SyntacticClass.Formats.InIso8601.Instances.UTCTime where

import qualified Attoparsec.Data as AttoparsecData
import Data.Time
import SyntacticClass.Core
import SyntacticClass.Formats.InIso8601.Core
import SyntacticClass.Prelude
import qualified TextBuilderTime.Iso8601 as Builders

-- | UTCTime in ISO-8601 format.
--
-- >>> toText (InIso8601 (UTCTime (fromGregorian 2021 11 24) (12 * 60 * 60 + 11 * 60 + 2)))
-- "2021-11-24T12:11:02Z"
--
-- >>> maybeFromText "2021-11-24T12:11:02Z" :: Maybe (InIso8601 UTCTime)
-- Just "2021-11-24T12:11:02Z"
--
-- >>> toText (InIso8601 (UTCTime (fromGregorian 2000 12 31) 0.0000123))
-- "2000-12-31T00:00:00.0000123Z"
instance Syntactic (InIso8601 UTCTime) where
  toTextBuilder (InIso8601 base) = Builders.utcTime base

  attoparsecParserOf = fmap InIso8601 AttoparsecData.utcTimeInISO8601

-- | String literal in ISO-8601 format.
--
-- >>> fromString "2021-11-24T12:11:02Z" :: InIso8601 UTCTime
-- "2021-11-24T12:11:02Z"
--
-- Invalid inputs get parsed as the Unix epoch.
--
-- >>> fromString "z" :: InIso8601 UTCTime
-- "1970-01-01T00:00:00Z"
instance IsString (InIso8601 UTCTime) where
  fromString = fromMaybe def . maybeFromText . fromString
    where
      def = InIso8601 (UTCTime (fromGregorian 1970 1 1) 0)

-- | String literal in ISO-8601 format.
--
-- >>> show (InIso8601 (UTCTime (fromGregorian 2000 12 31) 0))
-- "\"2000-12-31T00:00:00Z\""
instance Show (InIso8601 UTCTime) where
  showsPrec d = showsPrec d . toText
