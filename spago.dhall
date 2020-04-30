{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "solitaire"
, dependencies =
  [ "ansi"
  , "argonaut"
  , "argonaut-generic"
  , "console"
  , "css"
  , "effect"
  , "enums"
  , "generics-rep"
  , "halogen"
  , "halogen-css"
  , "lcg"
  , "node-fs"
  , "node-readline"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "quickcheck"
  , "refs"
  , "transformers"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
