{-

   UrlParsers
   =============

   Various parsing helpers to grab structured data like FQNs and Hashes out of
   routes
-}


module Code.UrlParsers exposing (..)

import Code.Definition.Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.HashQualified exposing (HashQualified(..))
import Code.Perspective exposing (PerspectiveParams(..), RootPerspectiveParam(..))
import Parser exposing ((|.), (|=), Parser, backtrackable, keyword, succeed)



-- Parsers --------------------------------------------------------------------


fqn : Parser FQN
fqn =
    let
        segment =
            Parser.oneOf
                -- Special case ;. which is an escaped . (dot), since we also use
                -- ';' as the separator character between namespace FQNs and
                -- definition FQNs. (';' is not a valid character in FQNs and is
                -- safe as a separator/escape character).
                [ b (succeed "." |. s ";.")
                , b chompSegment
                ]

        chompSegment =
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile FQN.isValidUrlSegmentChar
    in
    Parser.map FQN.fromUrlList
        (Parser.sequence
            { start = ""
            , separator = "/"
            , end = ""
            , spaces = Parser.spaces
            , item = segment
            , trailing = Parser.Forbidden
            }
        )


fqnEnd : Parser ()
fqnEnd =
    Parser.symbol ";"


hash : Parser Hash
hash =
    let
        handleMaybe mHash =
            case mHash of
                Just h ->
                    Parser.succeed h

                Nothing ->
                    Parser.problem "Invalid hash"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map Hash.fromUrlString
        |> Parser.andThen handleMaybe


hq : Parser HashQualified
hq =
    Parser.oneOf
        [ b (succeed HashOnly |= hash)
        , b (succeed NameOnly |= fqn)
        ]


reference : Parser Reference
reference =
    Parser.oneOf
        [ b (succeed TermReference |. s "terms" |. slash |= hq)
        , b (succeed TypeReference |. s "types" |. slash |= hq)
        , b (succeed AbilityConstructorReference |. s "ability-constructors" |. slash |= hq)
        , b (succeed DataConstructorReference |. s "data-constructors" |. slash |= hq)
        ]


codebaseRef : Parser RootPerspectiveParam
codebaseRef =
    Parser.oneOf
        [ b (succeed Relative |. s "latest")
        , b (succeed Absolute |= hash)
        ]


perspectiveParams : Parser PerspectiveParams
perspectiveParams =
    Parser.oneOf
        [ b (succeed ByNamespace |= codebaseRef |. slash |. s "namespaces" |. slash |= fqn |. fqnEnd)
        , b (succeed ByNamespace |= codebaseRef |. slash |. s "namespaces" |. slash |= fqn)
        , b (succeed ByRoot |= codebaseRef)
        ]



-- Helpers --------------------------------------------------------------------


slash : Parser ()
slash =
    Parser.symbol "/"


b : Parser a -> Parser a
b =
    backtrackable


s : String -> Parser ()
s =
    keyword
