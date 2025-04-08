# Lawful typeclass for canonical formatting and parsing

Many types have a canonical textual notation. E.g., UUID, email address, URL, IP. Having such a notation implies being able to render such values to text and parsing from it. This library provides a lawful typeclass for such types.

There also are types which have multiple textual notations. E.g., a date can be represented in the ISO8601 format or in the RFC1123 format. Such types are also supported by this library with the help of newtype wrappers such as `InIso8601`.

In the Haskell community there exists a common practice of using `Read` and `Show` typeclasses for similar purposes, however, it is flawed. These typeclasses are not lawful and they promote representing values as Haskell expressions instead of domain-specific notations which people often expect from them. This leads to a mess surrounding the conventions around these classes with a notable example of the "time" library supplying instances which don't operate on Haskell expressions. Atop of that they operate on `String`. Consider this library as a solution to this problem.

The typeclass operates on composable builders and parsers making the instances reusable in a wide range of situations.
