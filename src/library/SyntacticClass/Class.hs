module SyntacticClass.Class where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import SyntacticClass.Prelude
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- |
-- Canonical syntax for a value with rendering and parsing capabilities.
--
-- === Laws
--
-- - @'maybeFromText' . 'toText' = 'Just'@ - For any value, parsing a result of rendering it always succeeds and produces a value that is equal to the original.
-- 
-- === Testing
--
-- For testing whether your instances conform to these laws, use 'syntacticProperties'.
class Syntactic value where
  -- | Compile the value to 'TextBuilder'.
  toTextBuilder :: value -> TextBuilder.TextBuilder

  -- | Attoparsec parser of the value.
  -- May be lenient.
  -- I.e., it may accept more than the canonical syntax.
  attoparsecParserOf :: Attoparsec.Parser value

-- | Compile the value to 'Text'.
toText :: (Syntactic value) => value -> Text
toText = TextBuilder.toText . toTextBuilder

-- | Try to parse the value from 'Text' producing 'Nothing' on failure.
maybeFromText :: (Syntactic value) => Text -> Maybe value
maybeFromText input =
  Attoparsec.parseOnly (attoparsecParserOf <* Attoparsec.endOfInput) input
    & \case
      Left _ -> Nothing
      Right value -> Just value

-- * Laws

-- | QuickCheck properties for validating instances of 'Syntactic' indexed by their names.
--
-- The instance is identified via the proxy type that you provide.
--
-- E.g., here's how you can integrate it into an Hspec test-suite:
--
-- > spec = do
-- >   describe "Syntactic laws" do
-- >     traverse_ (uncurry prop) (syntacticProperties @UUID Proxy)
syntacticProperties ::
  forall value.
  (Syntactic value, Show value, Eq value, Arbitrary value) =>
  Proxy value ->
  -- | List of properties by names.
  [(String, QuickCheck.Property)]
syntacticProperties _ =
  [ ( "Parsing a rendered value produces the original",
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
