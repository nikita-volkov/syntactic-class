module SyntacticClass.Formats.InFixedBinary.Attoparsec where

import Data.Attoparsec.Text
import SyntacticClass.Prelude

-- |
-- Parser for a fixed-width binary number. The number of bits is determined
-- by the type of the value being parsed. The parser will consume exactly
-- the number of bits specified by the type, and will fail if there are fewer bits in the input.
--
-- >>> parseOnly @Int8 fixedWidthDigits "00000010"
-- Right 2
--
-- >>> parseOnly @Int8 fixedWidthDigits "00000001"
-- Right 1
--
-- Handles negative numbers:
--
-- >>> parseOnly @Int8 fixedWidthDigits "11111111"
-- Right (-1)
--
-- Handles larger inputs:
--
-- >>> parseOnly @Int8 fixedWidthDigits "0000000101"
-- Right 1
--
-- Does not handle smaller inputs:
--
-- >>> parseOnly @Int8 fixedWidthDigits "0000001"
-- Left "not enough input"
fixedWidthDigits :: forall a. (FiniteBits a, Num a) => Parser a
fixedWidthDigits =
  go (pred (finiteBitSize (undefined :: a))) 0
  where
    go i acc =
      if i < 0
        then return acc
        else do
          c <-
            satisfy \case
              '0' -> True
              '1' -> True
              _ -> False
          if c == '0'
            then go (pred i) acc
            else go (pred i) (setBit acc i)

-- |
-- A lax version of 'fixedWidthDigits' that allows for leading zeros to be missing.
--
-- >>> parseOnly @Int8 dynamicWidthDigits "00000010"
-- Right 2
--
-- >>> parseOnly @Int8 dynamicWidthDigits "00000001"
-- Right 1
--
-- Handles negative numbers:
--
-- >>> parseOnly @Int8 dynamicWidthDigits "11111111"
-- Right (-1)
--
-- Handles larger inputs:
--
-- >>> parseOnly @Int8 dynamicWidthDigits "0000000101"
-- Right 1
--
-- Handles smaller inputs:
--
-- >>> parseOnly @Int8 dynamicWidthDigits "0000001"
-- Right 1
--
-- >>> parseOnly @Word8 dynamicWidthDigits "11"
-- Right 3
--
-- Handles empty input:
--
-- >>> parseOnly @Int8 dynamicWidthDigits ""
-- Right 0
--
-- __Warning__
--
-- Due to \"sign extension\" it works differently on signed numbers:
--
-- >>> parseOnly @Int8 dynamicWidthDigits "11"
-- Right (-1)
--
-- To get the expected behaviour parse an unsigned analogue of the number and convert using 'fromIntegral' afterwards.
dynamicWidthDigits :: forall a. (FiniteBits a, Num a) => Parser a
dynamicWidthDigits =
  go (pred (finiteBitSize (undefined :: a))) 0
  where
    go i acc =
      if i < 0
        then return acc
        else do
          c <-
            optional $ satisfy $ \case
              '0' -> True
              '1' -> True
              _ -> False
          case c of
            Just c ->
              if c == '0'
                then go (pred i) acc
                else go (pred i) (setBit acc i)
            Nothing ->
              return (unsafeShiftR acc (succ i))
