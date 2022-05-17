{ name = "roundtip-test"
, dependencies =
  [ "aeson"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "aeson-helpers"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  , "typelevel-prelude"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "generated/**/*.purs" ]
}
