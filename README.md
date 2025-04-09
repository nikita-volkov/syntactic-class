# syntactic-class: Canonical Formatting and Parsing for Haskell

**syntactic-class** is a Haskell library that provides a lawful typeclass for canonical formatting and parsing of types. It overcomes the shortcomings of `Show` and `Read`, delivering a modern, efficient, and consistent way to render and parse types in their standard textual forms.

---

## 🔍 Problem

Haskell developers frequently turn to `Show` and `Read`, but these typeclasses come with significant drawbacks:

- **Unclear Purpose**: They’re lawless. The [docs](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Show) suggest rendering as Haskell expressions, but this isn’t enforced. Libraries like "time" [deviate](https://github.com/haskell/time/issues/271), leading to widespread inconsistency.
- **Confusing Output**: Newcomers find quoted values puzzling, and libraries like "optparse-applicative" propagate this confusion into app interfaces with default [formatting](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [parsing](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto).
- **Poor UX**: Built on outdated abstractions like `ShowS`, `ReadS`, and `ReadPrec`, they lack the elegance of modern monadic parsers and monoidal builders.
- **Inefficiency**: They operate on `String`, which says it all.

---

## 💡 Insight

Let’s be real: `Show` and `Read` are best suited for debugging and `ghci` tinkering. For practical use, we need tailored approaches:

1. **Canonical Notations**: Types like UUIDs, email addresses, URLs, and IPs have unambiguous, standard textual forms that imply both rendering and parsing.
2. **Multiple Notations**: Dates (e.g., ISO8601 or RFC1123) and integers (e.g., decimal or hexadecimal) can take multiple forms. Newtype wrappers offer a clean way to manage these variations.
3. **Complex Structures**: Structured data (e.g., JSON, YAML) is handled by libraries like "aeson" or custom pretty-printers.

**syntactic-class** tackles cases 1 and 2 with a lawful typeclass for canonical syntax, unrestricted by Haskell-specific constraints.

---

## ✨ Solution

The `Syntactic` typeclass ties types to their canonical textual representations, offering:

- **Lawfulness**: Parsing a rendered value always recovers the original (`maybeFromText . toText = Just`), ensuring reliability.
- **Canonical Focus**: Emphasizes standard textual forms, not Haskell expressions.
- **Efficiency**: Leverages strict "text-builder" for formatting and "attoparsec" for parsing, optimizing performance.
- **Flexibility**: Supports multiple notations via newtype wrappers (e.g., `InIso8601` for dates).
- **Ecosystem-Friendly**: Plays nicely with Haskell’s broader library ecosystem.
