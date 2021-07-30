module Hash exposing
    ( Hash
    , decode
    , equals
    , fromString
    , fromUrlString
    , isAbilityConstructorHash
    , isDataConstructorHash
    , isRawHash
    , prefix
    , toString
    , toUrlString
    , urlParser
    , urlPrefix
    )

import Json.Decode as Decode
import Regex
import Url.Parser
import Util


type Hash
    = Hash String


equals : Hash -> Hash -> Bool
equals (Hash a) (Hash b) =
    a == b


toString : Hash -> String
toString (Hash raw) =
    raw


fromString : String -> Maybe Hash
fromString raw =
    if String.startsWith prefix raw then
        Just (Hash raw)

    else
        Nothing


isRawHash : String -> Bool
isRawHash str =
    String.startsWith prefix str || String.startsWith urlPrefix str


fromUrlString : String -> Maybe Hash
fromUrlString str =
    if String.startsWith urlPrefix str then
        str
            |> String.replace urlPrefix prefix
            |> fromString

    else
        Nothing


toUrlString : Hash -> String
toUrlString hash =
    hash
        |> toString
        |> String.replace prefix urlPrefix


prefix : String
prefix =
    "#"


urlPrefix : String
urlPrefix =
    "@"



-- HELPERS


isDataConstructorHash : Hash -> Bool
isDataConstructorHash hash =
    let
        dataConstructorSuffix =
            Maybe.withDefault Regex.never (Regex.fromString "#d(\\d+)$")
    in
    hash |> toString |> Regex.contains dataConstructorSuffix


isAbilityConstructorHash : Hash -> Bool
isAbilityConstructorHash hash =
    let
        abilityConstructorSuffix =
            Maybe.withDefault Regex.never (Regex.fromString "#a(\\d+)$")
    in
    hash |> toString |> Regex.contains abilityConstructorSuffix



-- PARSERS


urlParser : Url.Parser.Parser (Hash -> a) a
urlParser =
    Url.Parser.custom "HASH" fromUrlString


decode : Decode.Decoder Hash
decode =
    Decode.map fromString Decode.string |> Decode.andThen (Util.decodeFailInvalid "Invalid Hash")
