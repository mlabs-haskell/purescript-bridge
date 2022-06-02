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

If you use either of these functions, the on-chain representation of any concrete instance of your type will be the `Constr index blah` representation, where `index` is the index you manually assigned to that constructor (if using `makeIsDataIndexed`) or the "default" constructor index (if using `unstableMakeIsData`). Consequently, you **MUST** use `mkPlutusDataType` when bridging types which are passed as arguments to these functions. (To use this function you must also import `PlutusTx.HasConstrIndices` to bring the `HasConstrIndices` class into scope.)

Minor note: If you already have some types which use the Plutus version of `unstableMakeIsData`, you typically do not need to use the version provided here, and can use `unsafeMkPlutusDataType` when bridging those types. This unsafe variant simply uses the default (i.e. the order in which constructors appear in the source code) ordering. (It is unsafe because there is no way, aside from looking at the source, to verify that the ToData and FromData instances for your type really do use the default ordering.)

### Special Plutus Translation Functions 

This project exposes an API for bridging user-defined types which is similar to the API provided by `purescript-bridge` (which this project started as a fork of). Indeed, the full `purescript-bridge` API is provided by this library, and you may use `mkSumType` for those types which never become on-chain Data. For those types which do have an on-chain representation, we provide a set of four functions which allow for correct translation to PureScript: 

**Note**: Each of the functions in this section generate `Show`, `ToData`, `FromData`, `EncodeAeson`, and `DecodeAeson` instances in PureScript. Additionally, if a Haskell type can be represented as a PureScript `newtype`, a `Newtype` instance will be derived. All of the Lens machinery generated by upstream `purescript-bridge` is also generated by these functions. 

#### `mkPlutusDataType`

This is the function that you want to use if you used `makeIsDataIndexed` or `unstableMakeIsData` (the versions provided by this library) to generate the Haskell `ToData/FromData` instances for your type. Using this function will cause `ctl-bridge` to generate PureScript `ToData/FromData` instances which encode to and decode from a `Constr index blah` on-chain representation. If you use `makeIsDataIndexed`, and **especially** if the constructor indices for your type have a non-default ordering, you should always use this. 

#### `mkPlutusNewtype`

This is the function you want to use if you derived the Haskell `ToData/FromData` instances using `GeneralizedNewtypeDeriving` or `DerivingVia`. If the instances were produced that way, PureScript needs to know to **not** use the `Constr` encoding for your type. 

**Note**: Do not assume that you wish to use this just because your type is a `newtype` in Haskell! It is possible to use `unstableMakeIsData` and `makeIsDataIndexed` to generate `ToData/FromData` instances for Haskell newtypes. This function simply communicates to PureScript that the on-chain representation of your type is identical to the on-chain representation of the type it wraps. (Conversely, you could use this function on a Haskell `data` type with a single constructor/argument... though I'm not sure why you would want to, as any type like that should probably be a `newtype` anyway.)

**Note (IMPORTANT!)**: For simplicity's sake, any Haskell `newtype` which is applied to this function will have its fields "erased" and will be transformed into a simple product newtype in PureScript. As an example, `newtype Foo = Foo {unFoo :: Bar}` becomes `newtype Foo = Foo Bar` in PureScript. This is not the default behavior of upstream `purescript-bridge`, so please keep this in mind. 

#### `unsafeMkPlutusDataType` 

This is the function you want to use if you used (the Plutus version of) `unstableMakeIsData`, i.e., if the constructors for your type have default indices. Again, this is unsafe because it is only possible to verify that the ToData and FromData instances for your type really do use the default ordering. For maximum safety, use (this library's version of) `unstableMakeIsData` and `mkPlutusDataType`. 

#### `mkPlutusNewtype_`

**Note (JSON)**: For each of the above functions in this section, the PureScript `Aeson` instances that are generated will conform with the default (i.e. generically-derived) Haskell `ToJSON/FromJSON` instances from `Data.Aeson`. 

Occasionally, you may wish to encode your a newtype into *JSON* using the JSON representation of the underlying type, typically by `deriving newtype (ToJSON,FromJSON)` or by using `DerivingVia`. Alternatively, you may be working with a Haskell type from some other library which uses one of those mechanisms to derive a JSON instance. `mkPlutusNewtype_` generates the same **on-chain** Data representation of your type as `mkPlutusNewtype`, but differs in the JSON instance: Using this function will give you a JSON instance that is identical to the instance for the underlying type. 

### Ledger Types & CTL Bridging 

Because user-defined types will often contain types from `Plutus.V1.Ledger.X` modules, this library also provides bridge-writing functions which, in addition to writing user-defined types, also write the Plutus ledger types so as to satisfy dependencies. 

**Note (IMPORTANT!)** For some types (`Value` is the most prominent example), CTL defines its own version which is not identical to the type that would be generated by `ctl-bridge`. Because this library is geared towards compatibility with CTL, each of the following functions overrides ledger types with their CTL equivalents so that CTL utilities for creating contracts & communicating with Cardano nodes can be used.

#### `writeLedgerTypes :: FilePath -> IO ()`

This function simply writes a copy of a PureScript version of the Plutus ledger types to the designated filepath. It does not translate any user defined types. 

#### `writeLedgerTypesAnd :: FilePath -> SumType 'Haskell -> IO ()`

This function writes a copy of the ledger types **and** translates all the user defined types provided by the second argument into PureScript modules at the designated location. This is probably the bridge-writing function that you will most frequently use. 

#### `writeLedgerTypesWithPartAnd :: FilePath -> BridgeBuilder PSType -> [SumType 'Haskell] -> IO ()`

This function is identical to `writeLedgerTypesAnd`, except it allows users to specify their own overrides using the `BridgeBuilder` provided in the second argument. See the `PlutusTx.LedgerTypes` module for some examples of overrides. 

### Examples 

A plethora of examples for how to use this are located in the `PlutusTx.LedgerTypes` module. While I hope the above documentation is sufficient for users to make use of this library, if you remain confused and cannot decide which function to use to translate your type, look at that module while referencing the Plutus source for the Ledger types that are translated therein. 

