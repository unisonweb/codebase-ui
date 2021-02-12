module FullyQualifiedName exposing (..)

import Json.Decode as Decode
import List.Nonempty exposing (Nonempty, last)


type FQN
    = FQN (Nonempty String)


{-| Turn a string, like "base.List.map" into FQN ["base", "List", "map"] |
-}
fromString : String -> FQN
fromString rawFqn =
    rawFqn
        |> String.split "."
        |> List.map String.trim
        |> List.filter (\s -> String.length s > 0)
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.fromElement ".")
        |> FQN


unqualifiedName : FQN -> String
unqualifiedName (FQN nameParts) =
    last nameParts



-- JSON DECODE


decode : Decode.Decoder FQN
decode =
    Decode.map fromString Decode.string
