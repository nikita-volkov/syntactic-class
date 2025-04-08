module SyntacticClass.Class where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import SyntacticClass.Prelude
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilderDev

-- |
-- Canonical syntax for a value with rendering and parsing capabilities.
--
-- Laws:
--
-- - @'maybeFromText' . 'toText' = 'Just'@
class Syntactic value where
  -- | Compile the value to 'TextBuilder'.
  toTextBuilder :: value -> TextBuilderDev.TextBuilder

  -- | Attoparsec parser of the value.
  attoparsecParserOf :: Attoparsec.Parser value

toText :: (Syntactic value) => value -> Text
toText = TextBuilderDev.to . toTextBuilder

maybeFromText :: (Syntactic value) => Text -> Maybe value
maybeFromText input =
  Attoparsec.parseOnly (attoparsecParserOf <* Attoparsec.endOfInput) input
    & \case
      Left _ -> Nothing
      Right value -> Just value

-- * Laws

-- | Properties for validating instances of 'Syntactic'.
syntacticProperties ::
  forall value.
  (Syntactic value, Show value, Eq value, Arbitrary value) =>
  Proxy value ->
  -- | List of properties by names.
  [(String, QuickCheck.Property)]
syntacticProperties _ =
  [ ( "Parsing a rendered value produces the original value",
      QuickCheck.property \(value :: value) ->
        let rendering = toText value
         in QuickCheck.counterexample
              ("Rendering: " <> Text.unpack rendering)
              case maybeFromText @value rendering of
                Nothing ->
                  QuickCheck.counterexample
                    "Rendering is unparsable"
                    (QuickCheck.property False)
                Just parsing ->
                  QuickCheck.counterexample
                    ("Parsing: " <> show parsing)
                    (parsing QuickCheck.=== value)
    )
  ]
