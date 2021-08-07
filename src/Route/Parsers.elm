{-

   Route.Parsers
   =============

   Various parsing helpers to grab structured data like FQNs and Hashes out of
   routes
-}


module Route.Parsers exposing (..)

import Definition.Reference exposing (Reference(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified exposing (HashQualified(..))
import Parser exposing ((|.), (|=), Parser, backtrackable, keyword, succeed)
import Perspective exposing (CodebasePerspectiveParam(..), PerspectiveParams(..))



-- Parsers --------------------------------------------------------------------


fqn : Parser FQN
fqn =
    let
        segment =
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile Char.isAlphaNum
    in
    Parser.map FQN.fromList
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
    Parser.symbol "-"


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


codebaseRef : Parser CodebasePerspectiveParam
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
        , b (succeed ByCodebase |= codebaseRef)
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
