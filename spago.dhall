{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "advent-of-code"
, dependencies =
  [ "aff"
  , "ansi"
  , "bignumber"
  , "console"
  , "debug"
  , "effect"
  , "foreign-object"
  , "free"
  , "functors"
  , "node-fs"
  , "node-readline"
  , "optparse"
  , "parseint"
  , "parsing"
  , "psci-support"
  , "strings"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
