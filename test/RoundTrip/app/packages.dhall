{-

Use localOverrides if you want to point Spago to a library like CTL to a local path.
Just add `// localOverrides` as in

```
let upstream =
https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall
sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

let Location =
/nix/store/255k1sr14k2v3bm0qk2i7dipxn8aqvwv-dhall-prelude-location

let localOverrides =
{ cardano-transaction-lib =
Location.Local "../../../../cardano-transaction-lib/spago.dhall"
}
//  (../../../../cardano-transaction-lib/spago.dhall).packages

in  upstream // ./locals.dhall // localOverrides
```
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall
        sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

in  upstream // ./locals.dhall
