# auto-parser

In Haskell, implementing a simple way to read your custom datatypes from text is as easy as writing `deriving Read` in the datatype definition. However, there does not seem to be an easy way to make this parsing method to provide some parsing diagnostics.

Potential solution: re-implement parsing the same format as `deriving Read` using parser combinators and datatype-generic programming. This repository is an attempt of implementing that solution, currently work-in-progress.

## Tasks

* [ ] Make AutoPrinter and AutoParser work for record types
* [ ] Make AutoPrinter and AutoParser work in presence of infix data constructors

## Notes

Generic representation of record type

```
ghci> :t from foo
from foo
  :: D1
       (MetaData "Foo" "Playground" "auto-parser-0.1.0.0-inplace" False)
       (C1
          (MetaCons "Foo" PrefixI True)
          (S1
             (MetaSel
                (Just "bar") NoSourceUnpackedness NoSourceStrictness DecidedLazy)
             (Rec0 Int)
           :*: (S1
                  (MetaSel
                     (Just "baz") NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                  (Rec0 Int)
                :*: S1
                      (MetaSel
                         (Just "quux") NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                      (Rec0 [Int]))))
```
