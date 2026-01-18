{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "random"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
