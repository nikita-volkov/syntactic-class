# syntactic-class

**A Lawful Typeclass for Canonical Formatting and Parsing in Haskell**

## Overview

**syntactic-class** is a Haskell library that provides a lawful typeclass for canonical formatting and parsing of types. It moves beyond the limitations of `Show` and `Read`, offering a modern, efficient, and consistent way to *render* and *parse* types in their standard textual forms.

## The Problem

Haskell’s default typeclasses, `Show` and `Read`, come with several limitations:

- **They are intended for Haskell expressions:**  
  Newcomers often find it confusing when shown types appear in quotes (e.g., [`UUID`](https://hackage.haskell.org/package/uuid-1.3.16/docs/Data-UUID.html#t:UUID)) or when such unexpected requirements leak to the interface of their apps due to libraries relying on them (e.g., "optparse-applicative" does so for default [formatting](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [parsing](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto) of arguments).

- **Inconsistency:**  
  There is no law requiring `Show` to be compatible with `Read`.
  The permissiveness of [docs](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Show) further leads to inconsistent implementations, which diverge from the suggested behavior (e.g., the `Show` instances in "time" [do not](https://github.com/haskell/time/issues/271) produce valid Haskell expressions).

- **Inefficiency:**  
  These classes operate on `String`, leading to performance issues.

- **Outdated user experience:**  
  The reliance on `ShowS`, `ReadS`, and `ReadPrec` results in clunky interfaces. Modern alternatives, such as "attoparsec" or "megaparsec" for parsing and monoidal builders for formatting, offer a smoother experience.

## The Insight

We recognize that:

- **Canonical representations exist:**  
  Many types—such as UUIDs, email addresses, URLs, and IP addresses—have a natural, canonical textual form. They will benefit from a standardized formatting and parsing approach.

- **Multiple notations are necessary:**  
  Some types support various representations. Dates might appear as ISO8601 or RFC1123, while integers might be rendered in decimal, hexadecimal, binary, or octal forms.

- **For structural data standard solutions already exist:**  
  Libraries such as "aeson" address complex structured data (products, sums, collections) using JSON, YAML, or similar formats, but a clear specification for simpler textual formats is still needed.

## The Solution

**syntactic-class** defines a typeclass that ties each type to its *canonical* textual representation, providing a uniform solution:

- **For types with a single representation:**  
  It directly delivers the canonical format.
  
- **For types with multiple representations:**  
  It encourages using newtype wrappers (e.g., a wrapper like `InIso8601` for dates) to distinguish between multiple valid formats.

**syntactic-class** is:

- **Lawful and consistent:**  
  Formats are determined solely by the type, without the encumbrance of Haskell expression encoding.

- **Optimized for efficiency:**  
  By using a monoidal "text-builder" for output and a monadic "attoparsec" parser for input, the implementation delivers a high-performance, scalable interface.

- **Composable and integrable:**  
  Designed to work seamlessly with existing Haskell libraries and to be extended with newtype wrappers for handling multiple formats, ensuring flexibility and reuse.

## Why syntactic-class?

- **Improved Clarity and Intent:**  
  You get a clear separation between debugging-oriented `Show`/`Read` and production-ready, canonical formatting/parsing.
  
- **Modern Interfaces:**  
  Replace clunky, outdated abstractions with modern functional programming tools.

- **Enhanced Developer Experience:**  
  The library minimizes confusion by establishing standard textual notations, making it easier for newcomers and experts alike to work with type representations.

## Conclusion

**syntactic-class** aims to be the go-to solution for canonical textual formatting and parsing in Haskell, addressing long-standing issues with traditional typeclasses. Whether you're writing a new library or updating an existing codebase, our approach helps establish clarity, efficiency, and modernity in your Haskell projects.

Happy coding!
