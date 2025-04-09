# syntactic-class

**A Lawful Typeclass for Canonical Formatting and Parsing in Haskell**

## Overview

**syntactic-class** provides a robust, composable, and lawful typeclass to bridge the gap between value rendering and parsing. It offers a modern, efficient alternative to Haskell’s built-in `Show` and `Read` typeclasses, delivering a standardized approach to textual representations for various types.

## The Problem

Haskell’s default typeclasses, `Show` and `Read`, come with several limitations:

- **Lack of Consistency:**  
  The `Show` typeclass is intended for rendering Haskell expressions, yet its permissiveness leads to inconsistent implementations. This inconsistency extends to many essential libraries (e.g., the `time` library) that diverge from the suggested behavior.

- **Inefficiency:**  
  Traditional implementations operate on `String` with suboptimal abstractions, leading to performance issues.

- **Outdated User Experience:**  
  The reliance on `ShowS`, `ReadS`, and `ReadPrec` results in clunky interfaces. Modern alternatives, such as "attoparsec" or "megaparsec" for parsing and monoidal builders for formatting, offer a smoother experience but are not integrated by default.

- **Unexpected Haskell Expressions:**  
  Newcomers often find it confusing when rendered types appear in extra quotes (e.g., UUIDs) or when domain-specific syntax leads to questions about representation (see examples like [`showDefault`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [`auto`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto) from *optparse-applicative*).

## The Insight

We recognize that:

- **Canonical Representations Exist:**  
  Many types—such as UUIDs, email addresses, URLs, and IP addresses—have a natural, canonical textual form. They benefit from a standardized formatting and parsing approach.

- **Multiple Notations are Necessary:**  
  Some types (e.g., dates or integers) support various representations. Dates might appear as ISO8601 or RFC1123, while integers might be rendered in decimal, hexadecimal, binary, or octal forms.

- **Standard Solutions for Structural Data:**  
  Libraries such as "aeson" address complex structured data (products, sums, collections) using JSON, YAML, or similar formats, but a clear specification for simpler textual formats is still needed.

By leveraging a lawful typeclass, **syntactic-class** provides an elegant solution:
- **For Types with a Single Representation:**  
  It directly delivers the canonical format.
  
- **For Types with Multiple Representations:**  
  It encourages using newtype wrappers (e.g., a wrapper like `InIso8601` for dates) to distinguish between multiple valid formats.

## The Solution

**syntactic-class** defines a typeclass that ties each type to its *canonical* textual representation:

- **Lawful and Consistent:**  
  Formats are determined solely by the type, without the encumbrance of Haskell expression encoding.

- **Optimized for Efficiency:**  
  By using a monoidal "text-builder" for output and a monadic "attoparsec" parser for input, the implementation delivers a high-performance, scalable interface.

- **Composable and Integrable:**  
  Designed to work seamlessly with existing Haskell libraries and to be extended with newtype wrappers for handling multiple formats, ensuring flexibility and reuse.

## Why syntactic-class?

- **Improved Clarity and Intent:**  
  You get a clear separation between debugging-oriented `Show`/`Read` and production-ready, canonical formatting/parsing.
  
- **Modern Interfaces:**  
  Replace clunky, outdated abstractions with modern functional programming tools.

- **Enhanced Developer Experience:**  
  The library minimizes confusion by establishing standard textual notations, making it easier for newcomers and experts alike to work with type representations.

## Getting Started

1. **Installation:**  
   Add `syntactic-class` to your project's dependencies using Cabal or Stack.

2. **Usage:**  
   Import the module and leverage the typeclass instances provided for common types. For types requiring multiple representations, wrap them with the appropriate newtype (e.g., `InIso8601` for dates).

3. **Contribution:**  
   We welcome contributions! Please refer to our contribution guidelines and issue tracker for more details.

## Conclusion

**syntactic-class** aims to be the go-to solution for canonical textual formatting and parsing in Haskell, addressing long-standing issues with traditional typeclasses. Whether you're writing a new library or updating an existing codebase, our approach helps establish clarity, efficiency, and modernity in your Haskell projects.

Happy coding!
