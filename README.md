# auto-parser

In Haskell, implementing a simple way to read your custom datatypes from text is as easy as writing `deriving Read` in the datatype definition. However, there does not seem to be an easy way to make this parsing method to provide some parsing diagnostics.

Potential solution: re-implement parsing the same format as `deriving Read` using parser combinators and datatype-generic programming.

A problem with potential solution: I don't really know how to use datatype-generic programming, so first, I'm reimplementing the functionality of `deriving Show` to familiarize myself with an API.
