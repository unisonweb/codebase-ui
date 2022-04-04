module Code.Hash exposing
    ( Hash
    , decode
    , equals
    , fromString
    , fromUrlString
    , isAbilityConstructorHash
    , isAssumedBuiltin
    , isDataConstructorHash
    , isRawHash
    , prefix
    , stripHashPrefix
    , toShortString
    , toString
    , toUrlString
    , unsafeFromString
    , urlParser
    , urlPrefix
    )

import Json.Decode as Decode
import Lib.Util as Util
import Regex
import Url.Parser


type Hash
    = Hash String


equals : Hash -> Hash -> Bool
equals (Hash a) (Hash b) =
    a == b


{-| TODO: Should this remove the prefix?
-}
toString : Hash -> String
toString (Hash raw) =
    raw


{-| Converts a Hash to a shortened (9 characters including the `#` character)
of the raw hash value.

Example:

  - Hash "#cv93ajol371idlcd47do5g3nmj7...4s829ofv57mi19pls3l630" -> "#cv93ajol"

Note that it does not shorten hashes that are assumed to be for builtins:

  - Hash "##Debug.watch" -> "##Debug.watch"
  - Hash "##IO.socketSend.impl" -> "##IO.SocketSend.impl"

-}
toShortString : Hash -> String
toShortString h =
    let
        shorten s =
            if isAssumedBuiltin h then
                s

            else
                String.left 9 s
    in
    h |> toString |> shorten


{-| Assuming a Hash string, it strips _any number_ of prefixes at the beginning
of the string

Examples:

  - "#abc123def456" -> "abc123def456"
  - "##IO.socketSend.impl" -> "IO.socketSend.impl"

This is often useful when rendering next to a Hash icon.

-}
stripHashPrefix : String -> String
stripHashPrefix s =
    let
        re =
            Maybe.withDefault Regex.never (Regex.fromString "^(#+)")
    in
    Regex.replace re (\_ -> "") s


fromString : String -> Maybe Hash
fromString raw =
    if String.startsWith prefix raw then
        Just (Hash raw)

    else
        Nothing


{-| !! Don't use this function outside of testing. It provides no guarantees
for the correctness of the Hash.
-}
unsafeFromString : String -> Hash
unsafeFromString raw =
    Hash raw


isRawHash : String -> Bool
isRawHash str =
    String.startsWith prefix str || String.startsWith urlPrefix str


{-| Checking a hash starts weith 2 `#` characters is a weak heuristic for
builtins, but sometimes useful.

  - Hash "##IO.socketSend.impl" -> True
  - Hash "##Debug.watch" -> True
  - Hash "#abc123def456" -> False

-}
isAssumedBuiltin : Hash -> Bool
isAssumedBuiltin hash_ =
    hash_ |> toString |> String.startsWith "##"


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
