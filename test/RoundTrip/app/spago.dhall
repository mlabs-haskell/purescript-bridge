{ name = "my-project"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "bigints"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "json-helpers"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
