# syntactic-class

Haskell library that provides a lawful typeclass for canonical formatting and parsing of types. It moves beyond the limitations of `Show` and `Read`, offering a modern, efficient, and consistent way to render and parse types in their standard textual forms.

---

## 🔍 Problem

Haskell developers often rely on `Show` and `Read`, but these typeclasses have notable issues:

- **Unclear Purpose**. The classes are lawless. The [docs](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Show) suggest rendering as Haskell expressions, but this is unenforced. Libraries like "time" [deviate](https://github.com/haskell/time/issues/271), causing cascading inconsistency.
- **Confusing Output**. Newcomers struggle with shown values being in quotes and many libraries leak this behaviour to the interfaces of apps (e.g., "optparse-applicative" does so with default [formatting](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [parsing](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto)).
- **Poor UX**. They lack the simplicity and power of modern monadic parsers and monoidal builders relying on outdated abstractions like `ShowS`, `ReadS` and `ReadPrec`.
- **Inefficiency**. They operate on `String`, which says it all.

## 💡 Insight

Let's face it: `Show` and `Read` are primarily for debugging and interacting with `ghci`. For other cases, we need different solutions:
1. **Many types have a canonical textual notation.** For example, there's no question how UUID, email address, URL, or IP must be represented in textual form. Having such a notation implies being able to render values to text and parse from it.
2. **Some types can have multiple textual notations.** For example, a date can be represented in ISO8601 format or RFC1123 format, and integers can be represented in decimal, hexadecimal, binary, or octal notations.
3. **Complex data structures** consisting of other structures (products, sums, collections) are typically represented using structural formats like JSON and YAML.

__Case 3__ is addressed by libraries like "aeson" and various pretty-printing libraries for custom formats.

__Case 1__ can be addressed with a lawful typeclass without restrictions on the format. __Case 2__ can be addressed by applying that typeclass to newtype wrappers. This project addresses both.

## ✨ Solution

A lawful typeclass that associates types with their canonical textual representation, ensuring of:

- **Lawfulness**: Requiring parser of a rendered value to produce the original ensures consistent, predictable behavior.
- **Canonical Focus**: Renders and parses types in their standard textual forms, free from Haskell expression constraints.
- **Efficiency**: Uses strict "text-builder" for formatting and "attoparsec" for parsing, ensuring efficiency and composability.
- **Flexibility**: Supports multiple notations via newtype wrappers (e.g., `InIso8601` for dates).
- **Ecosystem-Friendliness**: Integrates seamlessly with a wide range of existing Haskell libraries.
