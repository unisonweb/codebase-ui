module DefinitionMatch exposing (..)

import Definition exposing (Definition)
import Json.Decode as Decode exposing (field)
import Maybe.Extra as MaybeE


type alias DefinitionMatch =
    { score : Int
    , definition : Definition
    }



-- JSON DECODERS


decodeScore : Decode.Decoder Int
decodeScore =
    field "score" Decode.int


{-| Missing definitions result in decode failure, but we don't want a single
failure to break the entire result.

TODO: Is there some better combinators that work directly with Decode instead
of awkwardly going through Maybe?

-}
decodeMatch : Decode.Decoder (Maybe DefinitionMatch)
decodeMatch =
    Decode.oneOf
        [ Decode.map Just
            (Decode.map2
                DefinitionMatch
                (Decode.index 0 decodeScore)
                (Decode.index 1 Definition.decodeHead)
            )
        , Decode.succeed Nothing
        ]


decodeMatches : Decode.Decoder (List DefinitionMatch)
decodeMatches =
    Decode.map MaybeE.values (Decode.list decodeMatch)
