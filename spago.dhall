{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foreign"
  , "foreign-generic"
  , "halogen"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "random"
  , "tailrec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
