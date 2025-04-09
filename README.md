# syntactic-class: Canonical Formatting and Parsing for Haskell

Haskell library that provides a lawful typeclass for canonical formatting and parsing of types. It moves beyond the limitations of `Show` and `Read`, offering a modern, efficient, and consistent way to render and parse types in their standard textual forms.

---

## 📖 Introduction

Formatting and parsing are essential in programming, but Haskell's default tools, `Show` and `Read`, often fall short outside debugging. **syntactic-class** introduces a better solution: a typeclass that ties types to their *canonical* textual representations. Whether you're handling UUIDs, dates, or custom types, **syntactic-class** ensures clarity, efficiency, and composability.

With **syntactic-class**, you get:
- A lawful, predictable interface for formatting and parsing.
- Support for canonical textual notations (e.g., ISO8601 for dates).
- Integration with modern tools like "attoparsec" and "text-builder".

---

## 🚀 Why "syntactic-class"?

### The Trouble with `Show` and `Read`
Haskell developers often rely on `Show` and `Read`, but these typeclasses have notable issues:
- **Unclear Purpose**: The types are lawless. The [docs](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Show) suggest rendering as Haskell expressions, but this is unenforced. Libraries like "time" [deviate](https://github.com/haskell/time/issues/271), causing inconsistency.
- **Inefficiency**: They use `String` and outdated abstractions like `ShowS` and `ReadS`, not modern, efficient tools.
- **Confusing Output**: Newcomers struggle with shown values being in quotes or when such behaviour leaks to the interfaces of their apps due to libraries relying on them (e.g., "optparse-applicative" does so for [formatting](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [parsing](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto) defaults).
- **Poor UX**: They lack the simplicity and power of modern monadic parsers and monoidal builders.

### A Better Approach
- **Debugging vs. Real Use**: `Show` and `Read` suit debugging and `ghci`, but canonical formatting deserves a dedicated solution.
- **Canonical Notations**: Types like UUIDs, emails, or IPs have well-defined textual forms that imply both rendering and parsing.
- **Multiple Formats**: Types like dates or integers can have several notations (e.g., ISO8601 or RFC1123), manageable with newtype wrappers.
- **Complex Structures**: For JSON or YAML, libraries like "aeson" shine—**syntactic-class** focuses on canonical text.

**syntactic-class** addresses these challenges with a lawful, practical approach.

---

## ✨ Features

- **Lawful Typeclass**: Requiring parser of a rendered value to produce the original ensures consistent, predictable behavior.
- **Canonical Focus**: Renders and parses types in their standard textual forms, free from Haskell expression constraints.
- **Modern Tools**: Uses strict "text-builder" for formatting and "attoparsec" for parsing, ensuring efficiency and composability.
- **Flexible Formats**: Supports multiple notations via newtype wrappers (e.g., `InIso8601` for dates).
- **Ecosystem-Friendly**: Integrates seamlessly with existing Haskell libraries.

---

## 📚 Usage

### Formatting UTCTime in ISO-8601

The following app outputs `2000-12-31T00:01:01.0000123Z`.

```haskell
import qualified SyntacticClass as Syntactic
import qualified Data.Time as Time
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

timestamp :: Time.UTCTime
timestamp =
  Time.UTCTime
    (Time.fromGregorian 2000 12 31)
    61.0000123

timestampInIso8601 :: Syntactic.InIso8601 Time.UTCTime
timestampInIso8601 =
  Syntactic.InIso8601 timestamp

timestampText :: Text.Text
timestampText =
  Syntactic.toText timestampInIso8601

main :: IO ()
main =
  Text.putStrLn timestampText
```

### Controlling Show instances

The following app outputs `"2000-12-31T00:01:01.0000123Z"`.

```haskell
import qualified SyntacticClass as Syntactic
import qualified Data.Time as Time
import qualified Data.Text as Text

timestamp :: Time.UTCTime
timestamp =
  Time.UTCTime
    (Time.fromGregorian 2000 12 31)
    61.0000123

timestampInIso8601 :: Syntactic.InIso8601 Time.UTCTime
timestampInIso8601 =
  Syntactic.InIso8601 timestamp

main :: IO ()
main =
  print timestampInIso8601
```

### Parsing Text

The following app outputs `Just 2000-12-31 00:01:01.0000123 UTC`.

```haskell
import Data.Coerce
import qualified SyntacticClass as Syntactic
import qualified Data.Time as Time
import qualified Data.Text as Text

parsedTimestampInIso8601 :: Maybe (Syntactic.InIso8601 Time.UTCTime)
parsedTimestampInIso8601 =
  Syntactic.maybeFromText "2000-12-31T00:01:01.0000123Z"

parsedTimestamp :: Maybe Time.UTCTime
parsedTimestamp =
  coerce parsedTimestampInIso8601

main :: IO ()
main =
  print parsedTimestamp
```

---

## 🔍 How It Works

**syntactic-class** defines a typeclass with two core operations:
- **`toTextBuilder`**: Converts a value to its canonical notation using a monoidal builder.
- **`attoparsecParserOf`**: "attoparsec" parser of a value from its canonical notation.

The typeclass enforces laws (e.g., parsing a formatted value returns the original), ensuring reliability. By decoupling from Haskell expressions, it offers a cleaner, more practical solution.

The library also provides self-evident helper functions like `toText` and `maybeFromText`, which you've seen used in the examples.

---

## 🤝 Contributing

There's a lot of formats to be covered so moving further in that direction is most valuable. The codebase has a clear conventional structure, so just [do as the Romans do](https://en.wikipedia.org/wiki/When_in_Rome,_do_as_the_Romans_do).

---

## 📄 License

**syntactic-class** is licensed under the [MIT License](link-to-license).
