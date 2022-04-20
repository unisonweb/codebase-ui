module ReviewConfig exposing (config)

-- import NoUnused.Dependencies

import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ -- Removed, because of dependencies from ui-core that needs to be in the
      -- application's elm.json, which elm-review will complain about
      -- NoUnused.Dependencies.rule
      NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    ]
