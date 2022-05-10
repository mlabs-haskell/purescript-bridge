# purescript-quickcheck-combinators

This library is for generating `Unfoldable`s with trivial
constraints on the length of the structure generated - "at least",
"at most", and "between"; each indexed by a type-level natural number.

For instance:

```purescript
arbitraryArrayWithAtLeast5Elements :: forall a. Arbitrary a => Gen (Array a)
arbitraryArrayWithAtLeast5Elements = do
  AtLeast xs <- arbitrary :: Gen (AtLeast D5 Array a)
  pure xs
```

The only requirement is that the structure needs to be `Unfoldable` and `Traversable`,
apart from `a` needing to be `Arbitrary` as well.
