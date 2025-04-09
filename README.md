# Summary

Lawful typeclass for canonical formatting and parsing.

# Problem

In Haskell there is a lot of confusion and problems surrounding the problem area of having formatters and parsers associated with types. By default people reach towards `Show` and `Read` from "base" only to discover that:

1. They have no clear purpose, because there is no standard. [The docs](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Show) suggest to represent values as Haskell expressions, but they are stated permissively and don't declare any laws. As the result library authors interpret them freely leading to even some essential libraries such as "time" violating the suggested format leading to types using it having to violate it as well.

1. They are grossly inefficient. They operate on `String` and suboptimal abstractions.

1. They are outdated and have terrible UX. That `ShowS` instead of simple monoidal builders and those `ReadS` and `ReadPrec` instead of commonly accepted modern parsers like "attoparsec" or "megaparsec".

1. Now that choice of Haskell expressions as the suggested format leads to an endless stream of questions from newcommers being confused by `show` putting their rendered types in extra quotes (e.g., UUID). And libraries relying on them for domain-specific syntax bringing similar confusion (e.g., [`showDefault`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:showDefault) and [`auto`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/Options-Applicative.html#v:auto) of "optparse-applicative").

# Insight

Let's face it `Show` and `Read` are for debugging and dealing with `ghci`. That's it. For other cases we need different solutions. What other cases are there?

1. Many types have a canonical textual notation. E.g., there is no question how UUID, email address, URL or IP must be represented in textual form. Having such a notation implies being able to render such values to text and parsing from it. 

2. There also are types which can have multiple textual notations. E.g., a date can be represented in the ISO8601 format or in the RFC1123 format, an integer can be represented in decimal, hexadecimal, binary or octal notations.

3. There also are complex data structures consisting from other structures (products, sums, collections). For that we have various structural formats like JSON and YAML.

__Case 3__ gets addressed by libraries like "aeson" and various prettyprinting libraries for custom formats.

__Case 1__ can be addressed with a lawful typeclass without restrictions on the format. __Case 2__ can be addressed by applying that typeclass to newtype-wrappers. This project addresses them both.

# Solution

A lawful typeclass associating the type with a canonical textual representation of that type. The format is only determined by the type, so there's no requirements like needing it to be a Haskell expression.

For types that can have multiple formats associated with them newtype wrappers are supposed to be provided. E.g., because a date can be represented not only in the ISO8601 format a newtype wrappers named `InIso8601` with according instances is provided.

The typeclass operates on a monoidal "text-builder" and "attoparsec" parser aiming to provide an optimal composable generic interface integratable with a variety of existing solutions.
