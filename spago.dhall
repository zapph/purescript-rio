{ name = "rio"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "psci-support"
  , "transformers"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
