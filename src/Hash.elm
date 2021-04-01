module Hash exposing
    ( Hash
    , decode
    , equals
    , fromString
    , fromUrlString
    , isRawHash
    , toString
    , toUrlString
    , urlParser
    )

import Json.Decode as Decode
import Url.Parser


type Hash
    = Hash String


equals : Hash -> Hash -> Bool
equals (Hash a) (Hash b) =
    a == b


toString : Hash -> String
toString (Hash raw) =
    raw


fromString : String -> Hash
fromString raw =
    Hash raw


isRawHash : String -> Bool
isRawHash str =
    String.startsWith prefix str || String.startsWith urlPrefix str


fromUrlString : String -> Maybe Hash
fromUrlString str =
    if String.startsWith urlPrefix str then
        str
            |> String.replace urlPrefix prefix
            |> fromString
            |> Just

    else
        Nothing


toUrlString : Hash -> String
toUrlString hash =
    hash
        |> toString
        |> String.replace prefix urlPrefix



-- PARSERS


urlParser : Url.Parser.Parser (Hash -> a) a
urlParser =
    Url.Parser.custom "HASH" fromUrlString


decode : Decode.Decoder Hash
decode =
    Decode.map fromString Decode.string



-- INTERNAL


prefix : String
prefix =
    "#"


urlPrefix : String
urlPrefix =
    "@"
