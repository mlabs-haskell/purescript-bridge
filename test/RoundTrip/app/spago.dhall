{ name = "roundtip-test"
, dependencies =
  [ "aeson"
  , "aeson-helpers"
  , "bigints"
  , "cardano-transaction-lib"
  , "console"
  , "contravariant"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "generated/**/*.purs" ]
}
