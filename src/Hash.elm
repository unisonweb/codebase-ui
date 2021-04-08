module Hash exposing
    ( Hash
    , decode
    , equals
    , fromString
    , fromUrlString
    , isRawHash
    , prefix
    , toString
    , toUrlString
    , urlParser
    , urlPrefix
    )

import Json.Decode as Decode
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



-- PARSERS


urlParser : Url.Parser.Parser (Hash -> a) a
urlParser =
    Url.Parser.custom "HASH" fromUrlString


decode : Decode.Decoder Hash
decode =
    Decode.map fromString Decode.string |> Decode.andThen (Util.decodeFailInvalid "Invalid Hash")
