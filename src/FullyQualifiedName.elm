module FullyQualifiedName exposing
    ( FQN
    , decode
    , decodeFromParent
    , equals
    , fromParent
    , fromString
    , toString
    , unqualifiedName
    )

import Json.Decode as Decode
import List.Nonempty as NEL


type FQN
    = FQN (NEL.Nonempty String)



-- HELPERS


{-| Turn a string, like "base.List.map" into FQN ["base", "List", "map"]
-}
fromString : String -> FQN
fromString rawFqn =
    let
        rootEmptyToDot i s =
            if i == 0 && String.isEmpty s then
                "."

            else
                s
    in
    rawFqn
        |> String.split "."
        |> List.map String.trim
        |> List.indexedMap rootEmptyToDot
        |> List.filter (\s -> String.length s > 0)
        |> NEL.fromList
        |> Maybe.withDefault (NEL.fromElement ".")
        |> FQN


toString : FQN -> String
toString (FQN nameParts) =
    let
        -- Absolute FQNs start with a dot, so when also
        -- joining parts using a dot, we get dot dot (..),
        -- which we don't want.
        trimLeadingDot str =
            if String.startsWith ".." str then
                String.dropLeft 1 str

            else
                str
    in
    nameParts
        |> NEL.toList
        |> String.join "."
        |> trimLeadingDot


fromParent : FQN -> String -> FQN
fromParent (FQN parentParts) childName =
    FQN (NEL.append parentParts (NEL.fromElement childName))


unqualifiedName : FQN -> String
unqualifiedName (FQN nameParts) =
    NEL.last nameParts


equals : FQN -> FQN -> Bool
equals a b =
    toString a == toString b



-- JSON DECODE


decodeFromParent : FQN -> Decode.Decoder FQN
decodeFromParent parentFqn =
    Decode.map (fromParent parentFqn) Decode.string


decode : Decode.Decoder FQN
decode =
    Decode.map fromString Decode.string
