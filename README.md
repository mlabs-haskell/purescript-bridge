# ctl-bridge

## Purpose/Overview 

This project facilitates the translation of Haskell types to PureScript types in such a way that integrity of types in each language is maintained when interacting with the Cardano blockchain. The primary goal of this project is to ensure that the on-chain representation of equivalent types in each language is identical.

This library is not meant to be used in a standalone manner, and must be in concert with [ctl (cardano-transaction-lib)](https://github.com/plutonomicon/cardano-transaction-lib) to achieve on-chain compatibility between Haskell and PureScript types. 

ctl-bridge aims to solve 3 concrete problems which prevent the use of the unpatched `purescript-bridge` when on-chain compatibility is an issue. Those problems are: 

 1. Sum type constructor indexing. The on-chain data representation of a Haskell type [allows users to manually specify the index of a data type's constructor](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-IsData.html). This index information must be made available to PureScript to enable correct serialization and deserialization of on-chain data types. Without this information, it is impossible for PureScript to generate on-chain representations of types that match the Haskell representation.
 2. Record field ordering. `Record`s in PureScript are structurally distinct from Haskell records; in PureScript, records are backed by Rows, whereas in Haskell records are syntactic sugar over simple product types. As is typical in row-types implementations, PureScript internally sorts the fields of a record lexicographically (presumably for performance reasons). This presents a barrier to generically deriving ToData/FromData instances in PureScript. Because the existing PureScript generic machinery related to records only allows for accessing the fields of those records in the sorted order, we transfer information of the correct order of record fields to PureScript. 
 3. Aeson compatibility. The primary means of facilitating communication between PureScript and Haskell via Json (the `argonaut` PureScript library & friends) does not support `Integer/BigInt` decoding/encoding. ctl-bridge corrects this deficiency by generating instances for the `EncodeAeson/DecodeAeson` type classes in provided by the [purescript-aeson](https://github.com/mlabs-haskell/purescript-aeson) library.

## Usage 

_Note:_ In order to avoid errors which could lead to on-chain incompatibility, you must be aware of the manner in which the Haskell `ToData/FromData` instances for the type you wish to translate into PureScript were created. In particular, it is **essential** to keep in mind the difference between (on one hand) types which got their `ToData/FromData` instance from `unstableMakeIsData` or `makeIsDataIndexed`, and those which got their instances from `GeneralizedNewtypeDeriving` or `DerivingVia` clauses. 

**Every type which got its To/FromData instances from `unstableMakeIsData` or `makeIsDataIndexed` will be represented on-chain as `Constr index blah`**

**Every type which got its To/FromData instances from `GeneralizedNewtypeDeriving` or `DerivingVia` clauses will be represented on-chain in the same manner as the underlying type. You should _NOT_ use `unstableMakeIsData` or `makeIsDataIndexed` on these types.**

**You MUST choose the function that matches the manner in which your type got its `To/FromData` instances in Haskell**. This will be explained in detail below. (I apologize for the necessity of this warning, however there is no way around checking the source of your type's To/FromData instance by hand. It is an unfortunate consequence of IOHK's design choices for Plutus.)

Minor note: You should avoid writing `HasConstrIndices` instances by hand, though it may be necessary in cases where you are relying on 

### Template Haskell Hooks 

The `PlutusTx.Aux` module provides variants of the Plutus `unstableMakeIsData` and `makeIsDataIndexed` template-haskell functions. The variants provided by this library generate the same `ToData` and `FromData` instances in Haskell as the identically-named function from Plutus, but, additionally, generate instances of a `HasConstrIndices` class for their arguments. 

If you use either of these functions, the on-chain representation of any concrete instance of your type will be the `Constr index blah` representation, where `index` is the index you manually assigned to that constructor (if using `makeIsDataIndexed`) or the "default" constructor index (if using `unstableMakeIsData`). Consequently, you **MUST** use `mkPlutusDataType` when bridging types which are passed as arguments to these functions. 

Minor note: If you already have some types which use the Plutus version of `unstableMakeIsData`, you typically do not need to use the version provided here, and can use `unsafeMkPlutusDataType` when bridging those types. This unsafe variant simply uses the default (i.e. the order in which constructors appear in the source code) ordering. (It is unsafe because there is no way, aside from looking at the source, to verify that the ToData and FromData instances for your type really do use the default ordering.)

### Special Plutus Bridge Functions 

This project exposes an API for bridging user-defined types which is similar to the API provided by `purescript-bridge` (which this project started as a fork of). Indeed, the full `purescript-bridge` API is provided by this library, and you may use `mkSumType` for those types which never become on-chain Data. For those types which do have an on-chain representation, we provide a set of four functions which allow for correct translation to PureScript: 

#### `mkPlutusDataType`

This is the function that you want to use if you used `makeIsDataIndexed` (the version provided by this library). Using this function will cause `ctl-bridge` to generate PureScript `ToData/FromData` instances which encode to and decode from a `Constr index blah` on-chain representation. If you 

[![Build Status](https://travis-ci.org/eskimor/purescript-bridge.svg?branch=master)](https://travis-ci.org/eskimor/purescript-bridge)



Translate your Haskell types to PureScript types. It should in theory work for almost all Haskell types, including type constructors!
You just have to instantiate it with dummy parameters from e.g. "Language.PureScript.Bridge.TypeParameters".

Data type translation is fully and easily customizable by providing your own `BridgePart` instances!

## JSON encoding / decoding

For compatible JSON representations you should be using [aeson](http://hackage.haskell.org/package/aeson)'s generic encoding/decoding with default options
and `encodeJson` and `decodeJson` from "Data.Argonaut.Generic.Aeson" in [purescript-argonaut-generic-codecs](https://github.com/eskimor/purescript-argonaut-generic-codecs).


## Documentation

Usage of this library is documented in `Language.Purescript.Bridge`, with `writePSTypes` you should have everything to get started. Documentation can be found [here](https://www.stackage.org/nightly/package/purescript-bridge).

## Status

It works for my use case and is used in production. PRs for more `PSType`s definitions and bridges are very welcome! 
