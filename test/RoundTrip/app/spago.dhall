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
  , "enums"
  , "json-helpers"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  , "b64"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "generated/**/*.purs" ]
}
