# "syntactic-class": Canonical Formatting and Parsing for Haskell

**"syntactic-class"** is a Haskell library that provides a lawful typeclass for canonical formatting and parsing of types. It moves beyond the limitations of `Show` and `Read`, offering a modern, efficient, and consistent way to render and parse types in their standard textual forms—no Haskell expression syntax required.

---

## 📖 Introduction

Formatting and parsing are essential in programming, but Haskell's default tools, `Show` and `Read`, often fall short outside debugging. **"syntactic-class"** introduces a better solution: a typeclass that ties types to their *canonical* textual representations. Whether you're handling UUIDs, dates, or custom types, **"syntactic-class"** ensures clarity, efficiency, and composability.

With **"syntactic-class"**, you get:
- A lawful, predictable interface for formatting and parsing.
- Support for canonical textual notations (e.g., ISO8601 for dates).
- Integration with modern tools like `attoparsec` and monoidal text builders.

---

## 🚀 Why "syntactic-class"?

### The Trouble with `Show` and `Read`
Haskell developers often rely on `Show` and `Read`, but these typeclasses have notable issues:
- **Unclear Purpose**: The [docs](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Show) suggest rendering as Haskell expressions, but this is vague and unenforced. Libraries like "time" deviate, causing inconsistency.
- **Inefficiency**: They use `String` and outdated abstractions like `ShowS` and `ReadS`, not modern, efficient tools.
- **Confusing Output**: Newcomers struggle with quirks like extra quotes (e.g., UUIDs) or domain-specific syntax issues in libraries like "optparse-applicative" (see [`showDefault`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [`auto`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto)).
- **Poor UX**: They lack the simplicity and power of `attoparsec` or `megaparsec`.

### A Better Approach
- **Debugging vs. Real Use**: `Show` and `Read` suit debugging and `ghci`, but canonical formatting deserves a dedicated solution.
- **Canonical Notations**: Types like UUIDs, emails, or IPs have well-defined textual forms that imply both rendering and parsing.
- **Multiple Formats**: Types like dates or integers can have several notations (e.g., ISO8601 or RFC1123), manageable with newtype wrappers.
- **Complex Structures**: For JSON or YAML, libraries like "aeson" shine—**"syntactic-class"** focuses on canonical text.

**"syntactic-class"** addresses these challenges with a lawful, practical approach.

---

## ✨ Features

- **Lawful Typeclass**: Ensures consistent, predictable behavior.
- **Canonical Focus**: Renders and parses types in their standard textual forms, free from Haskell expression constraints.
- **Modern Tools**: Uses a monoidal text builder for formatting and `attoparsec` for parsing, ensuring efficiency and composability.
- **Flexible Formats**: Supports multiple notations via newtype wrappers (e.g., `InIso8601` for dates).
- **Ecosystem-Friendly**: Integrates seamlessly with existing Haskell libraries.

---

## 📚 Usage

### Basic Example
Define a `Syntactic` instance for a type like `UUID`:

```haskell
import Syntactic.Class

instance Syntactic UUID where
  format uuid = ...  -- Render to canonical text (e.g., "550e8400-e29b-41d4-a716-446655440000")
  parse = ...        -- Parse from canonical text
```

### Multiple Formats with Newtypes
For a `Date` with multiple representations:

```haskell
newtype InIso8601 a = In  = InIso8601 a

instance Syntactic (InIso8601 Date) where
  format (InIso8601 date) = ...  -- Render as ISO8601 (e.g., "2023-10-25")
  parse = ...                    -- Parse from ISO8601
```

This keeps representations clear and reusable.

---

## 🔍 How It Works

**"syntactic-class"** defines a typeclass with two core operations:
- **Formatting**: Converts a value to its canonical text using a monoidal builder.
- **Parsing**: Reads a value from its canonical text using an `attoparsec` parser.

The typeclass enforces laws (e.g., parsing a formatted value returns the original), ensuring reliability. By decoupling from Haskell expressions, it offers a cleaner, more practical solution.

---

## 📦 Getting Started

### Installation
Add **"syntactic-class"** to your project in your `.cabal` file or `package.yaml`:

```yaml
dependencies:
  - syntactic-class
```

### First Steps
1. Implement `Syntactic` instances for your types.
2. Use `format` and `parse` to handle canonical representations.

Check the [documentation](link-to-docs) for detailed guides and examples.

---

## 🤝 Contributing

We’d love your help to improve **"syntactic-class"**! Here’s how to get involved:
- Browse the [issue tracker](link-to-issues) for ideas or report bugs.
- Submit pull requests with enhancements or new features.
- Join the conversation on [GitHub Discussions](link-to-discussions).

See our [contributing guidelines](link-to-contributing) for more details.

---

## 📄 License

**"syntactic-class"** is licensed under the [MIT License](link-to-license).

---

## 🙏 Acknowledgements

Thanks to the Haskell community for inspiring this project and highlighting the need for a better formatting and parsing solution.

---

## 📬 Contact

Questions or ideas? Reach out:
- File an issue on GitHub.
- Email us at [your-email@example.com](mailto:your-email@example.com).
- Chat with us on [community chat](link-to-chat).

---

The library name is now consistently `syntactic-class`, and the typeclass is named `Syntactic` for clarity (aligned with Haskell naming conventions). Replace placeholder links with actual URLs before publishing on GitHub!