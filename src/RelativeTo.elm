module RelativeTo exposing (..)

import Hash exposing (Hash)


type RelativeTo
    = Codebase
    | Namespace Hash



-- HELPERS


toUrlPath : RelativeTo -> String
toUrlPath relTo =
    case relTo of
        Codebase ->
            "latest"

        Namespace h ->
            Hash.toUrlString h
