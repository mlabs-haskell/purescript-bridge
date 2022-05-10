{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quickcheck-combinators"
, dependencies =
  [ "psci-support", "quickcheck", "typelevel" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
