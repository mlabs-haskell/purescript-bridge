{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-bridge-typelib-cbtx"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "purescript-bridge-json-helpers"
  , "cardano-transaction-lib"
  , "enums"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
