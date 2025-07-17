module ReviewConfig exposing (config)

import NoUnused.Exports
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnused.Exports.rule
    ]
