# Cardax Nix

## Purescript bridge

`purescript-bridge-typelib.nix` library is used to create a derivation that
invokes the a user supplied program that receives a directory location and
generates the Purescript library with mapped Haskell types. The derivation also
performs a full Purescript compilation as a basic sanity check and that involves
using `purs` but also `spago` and `spago2nix`. That's the reason behind the
`./purescript-bridge-typelib-spago` directory.

### Upgrading Spago packages

The Purescript compilation expects a certain set of modules, most notably the
`cardano-browser-tx` Purescript modules. To make sure we're translating Haskell
types into the most recent Cardano Browser Tx Purescript types, it will be
necessary to perform `spago` upgrades.

```shell
$ cd nix/purescript-bridge-typelib-spago
$ edit spago.dhall and packages.dhall
$ make
$ git add spago-packages.nix
$ git commit -m "Upgraded Spago packages for the purescript-bridge-typelib.nix"
```
