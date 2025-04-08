module SyntacticClass.Instances.InIso8601 where

import qualified Attoparsec.Data as AttoparsecData
import Data.Time
import SyntacticClass.Class
import qualified SyntacticClass.Instances.InIso8601.Builders as Builders
import SyntacticClass.Prelude

newtype InIso8601 a = InIso8601 {base :: a}
  deriving newtype (Eq, Ord, Arbitrary)

-- * UTCTime

-- | UTCTime in ISO-8601 format.
--
-- >>> toText (InIso8601 (UTCTime (fromGregorian 2021 11 24) (12 * 60 * 60 + 11 * 60 + 2)))
-- "2021-11-24T12:11:02Z"
--
-- >>> maybeFromText "2021-11-24T12:11:02Z" :: Maybe (InIso8601 UTCTime)
-- Just "2021-11-24T12:11:02Z"
instance Syntactic (InIso8601 UTCTime) where
  toTextBuilder (InIso8601 base) = Builders.utcTime base

  attoparsecParserOf = fmap InIso8601 AttoparsecData.utcTimeInISO8601

instance IsString (InIso8601 UTCTime) where
  fromString = fromMaybe def . maybeFromText . fromString
    where
      def = InIso8601 (UTCTime (fromGregorian 1970 1 1) 0)

instance Show (InIso8601 UTCTime) where
  showsPrec d = showsPrec d . toText
