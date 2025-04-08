module SyntacticClass.Instances.InIso8601 where

import qualified Attoparsec.Time.Text as Attoparsec
import Data.Time
import SyntacticClass.Class
import qualified SyntacticClass.Instances.InIso8601.Builders as Builders
import SyntacticClass.Prelude
import qualified TextBuilderDev

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
  toTextBuilder (InIso8601 UTCTime {..}) =
    let (year, month, day) = toGregorian utctDay
        picoseconds = diffTimeToPicoseconds utctDayTime
        (seconds, picosecond) = divMod picoseconds 1_000_000_000_000
        seconds' = fromInteger seconds :: Int
        (dayMinutes, second) = divMod seconds' 60
        (hour, minute) = divMod dayMinutes 60
     in mconcat
          [ TextBuilderDev.fixedLengthDecimal 4 year,
            "-",
            TextBuilderDev.fixedLengthDecimal 2 month,
            "-",
            TextBuilderDev.fixedLengthDecimal 2 day,
            "T",
            TextBuilderDev.fixedLengthDecimal 2 hour,
            ":",
            TextBuilderDev.fixedLengthDecimal 2 minute,
            ":",
            TextBuilderDev.fixedLengthDecimal 2 second,
            Builders.picosecondsSubsecondsComponent (fromIntegral picosecond),
            "Z"
          ]

  attoparsecParserOf = fmap InIso8601 Attoparsec.utcTimeInISO8601

instance IsString (InIso8601 UTCTime) where
  fromString = fromMaybe def . maybeFromText . fromString
    where
      def = InIso8601 (UTCTime (fromGregorian 1970 1 1) 0)

instance Show (InIso8601 UTCTime) where
  showsPrec d = showsPrec d . toText
