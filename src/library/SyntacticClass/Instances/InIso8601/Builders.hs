module SyntacticClass.Instances.InIso8601.Builders where

import SyntacticClass.Prelude
import TextBuilderDev

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
