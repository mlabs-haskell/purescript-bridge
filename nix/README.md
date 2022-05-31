# Purescript bridge

`purescript-bridge.nix` library is used to create a derivation that
invokes the a user supplied program that receives a directory location and
generates the Purescript library with mapped Haskell types. The derivation also
performs a full Purescript compilation as a basic sanity check and that involves
using `purs` but also `spago` and `spago2nix`. That's the reason behind the
`./purescript-bridge-nix-spago` directory.

### Upgrading Spago packages

The Purescript compilation expects a certain set of modules, most notably the
`cardano-transaction-lib` Purescript modules. To make sure we're translating
Haskell types into the most recent Cardano Browser Transaction Lib Purescript
types, it will be necessary to perform `spago` upgrades.

```shell
$ cd nix/purescript-bridge-nix-spago
$ edit spago.dhall and packages.dhall
$ make
$ spago build && echo "should succeed"
$ git add spago-packages.nix
$ git commit -m "Upgraded Spago packages for the purescript-bridge.nix"
```
