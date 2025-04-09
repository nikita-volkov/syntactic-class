# Summary

Lawful typeclass for canonical formatting and parsing.

# Problem

In Haskell, there is considerable confusion surrounding formatters and parsers associated with types. By default, people reach for `Show` and `Read` from "base" and eventually discover that:

1. They have no clear purpose because there is no standard. [The docs](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Show) suggest representing values as Haskell expressions, but the guidelines are permissive and don't declare any laws. As a result, library authors interpret them freely, leading to even essential libraries such as "time" violating the suggested format.

2. They are grossly inefficient. They operate on `String` and use suboptimal abstractions.

3. They are outdated and have terrible UX. They use `ShowS` instead of simple monoidal builders and rely on `ReadS` and `ReadPrec` instead of commonly accepted modern parsers like "attoparsec" or "megaparsec".

4. The choice of Haskell expressions as the suggested format leads to an endless stream of questions from newcomers confused by `show` putting rendered types in extra quotes (e.g., UUID). Libraries relying on these for domain-specific syntax bring similar confusion (e.g., [`showDefault`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [`auto`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto) in "optparse-applicative").

# Insight

Let's face it: `Show` and `Read` are primarily for debugging and interacting with `ghci`. For other cases, we need different solutions:

1. Many types have a canonical textual notation. For example, there's no question how UUID, email address, URL, or IP must be represented in textual form. Having such a notation implies being able to render values to text and parse from it.

2. Some types can have multiple textual notations. For example, a date can be represented in ISO8601 format or RFC1123 format, and integers can be represented in decimal, hexadecimal, binary, or octal notations.

3. Complex data structures consisting of other structures (products, sums, collections) are typically represented using structural formats like JSON and YAML.

__Case 3__ is addressed by libraries like "aeson" and various pretty-printing libraries for custom formats.

__Case 1__ can be addressed with a lawful typeclass without restrictions on the format. __Case 2__ can be addressed by applying that typeclass to newtype wrappers. This project addresses both.

# Solution

A lawful typeclass that associates types with their canonical textual representation. The format is determined solely by the type, with no requirement to be a Haskell expression.

For types that can have multiple formats, newtype wrappers can be provided. For example, since dates can be represented in multiple formats, a newtype wrapper named `InIso8601` with appropriate instances is provided by the library.

The typeclass operates on a monoidal "text-builder" and monadic "attoparsec" parser, aiming to provide an optimal, composable, generic interface that integrates with various existing solutions.
