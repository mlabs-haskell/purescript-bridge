{ name = "roundtip-test"
, dependencies =
  [ "aeson"
  , "cardano-transaction-lib"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "node-readline"
  , "prelude"
  , "typelevel-prelude"
  , "untagged-union"
  , "purescript-bridge-typelib"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
