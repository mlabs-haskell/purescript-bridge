{ name = "my-project"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "json-helpers"
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
