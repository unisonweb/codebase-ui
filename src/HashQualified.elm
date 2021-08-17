module HashQualified exposing
    ( HashQualified(..)
    , fromString
    , fromUrlString
    , hash
    , name
    , toString
    , toUrlString
    , urlParser
    )

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Maybe.Extra as MaybeE
import Url.Parser



{--

  HashQualified: Definition Identifier
  ====================================

  The `NameOnly` variant doesn't mean that the definition doesn't have a
  `Hash`, but that we aren't using the `Hash` to identify it, same with
  `HashOnly`.

  The `HashQualified` variant is used when the name isn't enough to identify a
  definition; meaning there are multiple definitions with the same name.  To
  disambiguate a `Hash` is added in addition to the name.  The stringified
  example of this looks like so: `base.List.map@abc123`.

--}


type HashQualified
    = NameOnly FQN
    | HashOnly Hash
    | HashQualified FQN Hash



-- CREATE


fromString : String -> HashQualified
fromString str =
    str
        |> Hash.fromString
        |> Maybe.map HashOnly
        |> MaybeE.orElse (hashQualifiedFromString FQN.fromString Hash.prefix str)
        |> Maybe.withDefault (NameOnly (FQN.fromString str))


fromUrlString : String -> HashQualified
fromUrlString str =
    str
        |> Hash.fromUrlString
        |> Maybe.map HashOnly
        |> MaybeE.orElse (hashQualifiedFromString FQN.fromUrlString Hash.urlPrefix str)
        |> Maybe.withDefault (NameOnly (FQN.fromUrlString str))



-- PARSERS


urlParser : Url.Parser.Parser (HashQualified -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map HashOnly Hash.urlParser
        , Url.Parser.map NameOnly FQN.urlParser
        ]



-- HELPERS


name : HashQualified -> Maybe FQN
name hq =
    case hq of
        NameOnly fqn ->
            Just fqn

        HashOnly _ ->
            Nothing

        HashQualified fqn _ ->
            Just fqn


hash : HashQualified -> Maybe Hash
hash hq =
    case hq of
        NameOnly _ ->
            Nothing

        HashOnly h ->
            Just h

        HashQualified _ h ->
            Just h



-- TRANSFORMS


toString : HashQualified -> String
toString hq =
    case hq of
        NameOnly fqn ->
            FQN.toString fqn

        HashOnly hash_ ->
            Hash.toString hash_

        HashQualified fqn hash_ ->
            FQN.toString fqn ++ Hash.toString hash_


toUrlString : HashQualified -> String
toUrlString hq =
    case hq of
        NameOnly fqn ->
            FQN.toUrlString fqn

        HashOnly hash_ ->
            Hash.toUrlString hash_

        HashQualified fqn hash_ ->
            FQN.toUrlString fqn ++ Hash.toUrlString hash_



-- INTERNAL HELPERS


isRawHashQualified : String -> Bool
isRawHashQualified str =
    not (Hash.isRawHash str) && String.contains Hash.urlPrefix str


hashQualifiedFromString : (String -> FQN) -> String -> String -> Maybe HashQualified
hashQualifiedFromString toFQN sep str =
    if isRawHashQualified str then
        let
            parts =
                String.split sep str
        in
        case parts of
            [] ->
                Nothing

            [ "" ] ->
                Nothing

            "" :: _ ->
                Nothing

            name_ :: unprefixedHash :: [] ->
                Hash.fromString (Hash.prefix ++ unprefixedHash)
                    |> Maybe.map (HashQualified (toFQN name_))

            _ ->
                Nothing

    else
        Nothing
