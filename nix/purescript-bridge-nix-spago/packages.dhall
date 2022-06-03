{-

If you want to use a CTL repo local to your filesystem, change the corresponding input in `flake.nix`.

Specifically, instead of:

`cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/bladyjoker/use_aeson;`

use

`cardano-transaction-lib.url = path:/home/bladyjoker/Desktop/cardano-transaction-lib;`

-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall
        sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

in  upstream // ./locals.dhall
