module FullyQualifiedName exposing
    ( FQN
    , append
    , decode
    , decodeFromParent
    , equals
    , fromList
    , fromParent
    , fromString
    , fromUrlList
    , fromUrlString
    , isSuffixOf
    , isValidSegmentChar
    , isValidUrlSegmentChar
    , namespace
    , namespaceOf
    , numSegments
    , segments
    , toString
    , toUrlSegments
    , toUrlString
    , unqualifiedName
    , urlParser
    , view
    )

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import List.Extra as ListE
import List.Nonempty as NEL
import String.Extra as StringE
import Url
import Url.Parser


type FQN
    = FQN (NEL.Nonempty String)



-- HELPERS


{-| Turn a string, like "base.List.map" into FQN ["base", "List", "map"]

    Split text into segments. A smarter version of `Text.split` that handles
    the name `.` properly.

-}
fromString : String -> FQN
fromString rawFqn =
    let
        go s =
            case s of
                [] ->
                    []

                "" :: "" :: z ->
                    "." :: go z

                "" :: z ->
                    go z

                x :: y ->
                    x :: go y
    in
    rawFqn
        |> String.split "."
        |> go
        |> fromList


fromList : List String -> FQN
fromList segments_ =
    segments_
        |> List.map String.trim
        |> List.filter (String.isEmpty >> not)
        |> NEL.fromList
        |> Maybe.withDefault (NEL.fromElement ".")
        |> FQN


fromUrlString : String -> FQN
fromUrlString str =
    str
        |> String.split "/"
        |> fromUrlList


fromUrlList : List String -> FQN
fromUrlList segments_ =
    let
        urlDecode s =
            -- Let invalid % encoding fall through, since it then must be valid
            -- strings
            Maybe.withDefault s (Url.percentDecode s)
    in
    segments_
        |> List.map (urlDecode >> urlDecodeSegmentDot)
        |> fromList


toString : FQN -> String
toString (FQN nameParts) =
    let
        -- Absolute FQNs start with a dot, so when also
        -- joining parts using a dot, we get dot dot (..),
        -- which we don't want.
        -- TODO: this does mean that we don't support . as a term name on the root...
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


toUrlSegments : FQN -> NEL.Nonempty String
toUrlSegments fqn =
    fqn
        |> segments
        |> NEL.map (Url.percentEncode >> urlEncodeSegmentDot)


toUrlString : FQN -> String
toUrlString fqn =
    fqn
        |> toUrlSegments
        |> NEL.toList
        |> String.join "/"


segments : FQN -> NEL.Nonempty String
segments (FQN segments_) =
    segments_


numSegments : FQN -> Int
numSegments (FQN segments_) =
    NEL.length segments_


fromParent : FQN -> String -> FQN
fromParent (FQN parentParts) childName =
    FQN (NEL.append parentParts (NEL.fromElement childName))


unqualifiedName : FQN -> String
unqualifiedName (FQN nameParts) =
    NEL.last nameParts


namespace : FQN -> Maybe FQN
namespace (FQN segments_) =
    case segments_ |> NEL.toList |> ListE.init of
        Nothing ->
            Nothing

        Just [] ->
            Nothing

        Just segments__ ->
            Just (fromList segments__)


equals : FQN -> FQN -> Bool
equals a b =
    toString a == toString b


append : FQN -> FQN -> FQN
append (FQN a) (FQN b) =
    FQN (NEL.append a b)


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
            |> String.dropRight (String.length suffixName)
            |> StringE.nonEmpty
            |> Maybe.map dropLastDot

    else
        Nothing


view : FQN -> Html msg
view fqn =
    let
        viewSegment seg =
            span [ class "fully-qualified-name-segment" ] [ text seg ]

        viewSep =
            span [ class "fully-qualified-name-separator" ] [ text "." ]
    in
    fqn
        |> segments
        |> NEL.toList
        |> List.map viewSegment
        |> List.intersperse viewSep
        |> span [ class "fully-qualified-name" ]



-- PARSERS


urlParser : Url.Parser.Parser (FQN -> a) a
urlParser =
    Url.Parser.custom "FQN" (fromUrlString >> Just)


decodeFromParent : FQN -> Decode.Decoder FQN
decodeFromParent parentFqn =
    Decode.map (fromParent parentFqn) Decode.string


decode : Decode.Decoder FQN
decode =
    Decode.map fromString Decode.string


isValidSegmentChar : Char -> Bool
isValidSegmentChar c =
    let
        validSymbols =
            String.toList "!$%^&*-=+<>.~\\/:_'"
    in
    Char.isAlphaNum c || List.member c validSymbols


isValidUrlSegmentChar : Char -> Bool
isValidUrlSegmentChar c =
    -- '/' is a segment separator in Urls and
    -- should be escaped to %2F, so when
    -- unescaped, its not a valid segment
    -- character when parsing URLs.
    c /= '/' && isValidSegmentChar c



-- INTERNAL HELPERS


{-| URLs can't include a single dot in a path segment like so "base/./docs",
but this is a valid definition name in Unison, the composition operator for
example is named "." To get around this we encode dots as ";." in segments such
that "base...doc" becomes "base/;./doc"
-}
urlEncodeSegmentDot : String -> String
urlEncodeSegmentDot s =
    if s == "." then
        ";."

    else
        s


urlDecodeSegmentDot : String -> String
urlDecodeSegmentDot s =
    if s == ";." then
        "."

    else
        s
