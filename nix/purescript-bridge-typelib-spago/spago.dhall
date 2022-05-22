{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-bridge-typelib"
, dependencies =
  [ "aeson"
  , "aeson-helpers"
  , "cardano-transaction-lib"
  , "enums"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "generated/**/*.purs" ]
}
