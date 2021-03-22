module FullyQualifiedName exposing
    ( FQN
    , decode
    , decodeFromParent
    , equals
    , fromParent
    , fromString
    , isSuffixOf
    , namespaceOf
    , toString
    , unqualifiedName
    )

import Json.Decode as Decode
import List.Nonempty as NEL
import String.Extra as StringE


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


{-| This is passed through a string as a suffix name can include
namespaces like List.map (where the FQN would be
base.List.map)
-}
isSuffixOf : String -> FQN -> Bool
isSuffixOf suffixName fqn =
    String.endsWith suffixName (toString fqn)


{-| TODO: We should distinquish between FQN, Namespace and SuffixName on a type
level, or rename the FQN type to Name
-}
namespaceOf : String -> FQN -> Maybe String
namespaceOf suffixName fqn =
    let
        dropLastDot s =
            if String.endsWith "." s then
                String.dropRight 1 s

            else
                s
    in
    if isSuffixOf suffixName fqn then
        fqn
            |> toString
            |> String.replace suffixName ""
            |> StringE.nonEmpty
            |> Maybe.map dropLastDot

    else
        Nothing



-- JSON DECODE


decodeFromParent : FQN -> Decode.Decoder FQN
decodeFromParent parentFqn =
    Decode.map (fromParent parentFqn) Decode.string


decode : Decode.Decoder FQN
decode =
    Decode.map fromString Decode.string
