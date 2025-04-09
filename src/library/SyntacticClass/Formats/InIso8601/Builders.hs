module SyntacticClass.Formats.InIso8601.Builders
  ( day,
    utcTime,
  )
where

import SyntacticClass.Prelude
import TextBuilder

{-# INLINE day #-}
day :: Day -> TextBuilder
day (toGregorian -> (year, month, day)) =
  mconcat
    [ fixedLengthDecimal 4 year,
      "-",
      fixedLengthDecimal 2 month,
      "-",
      fixedLengthDecimal 2 day
    ]

{-# INLINE utcTime #-}
utcTime :: UTCTime -> TextBuilder
utcTime UTCTime {..} =
  let picoseconds = diffTimeToPicoseconds utctDayTime
      (seconds, picosecond) = divMod picoseconds 1_000_000_000_000
      seconds' = fromInteger seconds :: Int
      (dayMinutes, second) = divMod seconds' 60
      (hour, minute) = divMod dayMinutes 60
   in mconcat
        [ day utctDay,
          "T",
          fixedLengthDecimal 2 hour,
          ":",
          fixedLengthDecimal 2 minute,
          ":",
          fixedLengthDecimal 2 second,
          picosecondsSubsecondsComponent (fromIntegral picosecond),
          "Z"
        ]

-- |
-- Subseconds component of the ISO-8601 format compiled from picoseconds.
--
-- >>> picosecondsSubsecondsComponent 000_000_000_001
-- ".000000000001"
--
-- >>> picosecondsSubsecondsComponent 000_000_000_010
-- ".00000000001"
--
-- >>> picosecondsSubsecondsComponent 100_000_000_000
-- ".1"
--
-- >>> picosecondsSubsecondsComponent 0
-- ""
{-# INLINE picosecondsSubsecondsComponent #-}
picosecondsSubsecondsComponent ::
  -- | Picoseconds.
  Int ->
  TextBuilder
picosecondsSubsecondsComponent =
  skipTrail 12
  where
    skipTrail pos val =
      if val == 0
        then
          mempty
        else case divMod val 10 of
          (quotient, remainder) ->
            if remainder == 0
              then
                skipTrail (pred pos) quotient
              else
                "." <> fixedLengthDecimal pos val
