module Code.Definition.Reference exposing (..)

import Code.FullyQualifiedName exposing (FQN)
import Code.Hash exposing (Hash)
import Code.HashQualified as HQ exposing (HashQualified)
import UI.Icon as Icon exposing (Icon)
import Url.Parser


type Reference
    = TermReference HashQualified
    | TypeReference HashQualified
    | AbilityConstructorReference HashQualified
    | DataConstructorReference HashQualified



-- CREATE


fromString : (HashQualified -> Reference) -> String -> Reference
fromString toRef str =
    str |> HQ.fromString |> toRef


fromUrlString : (HashQualified -> Reference) -> String -> Reference
fromUrlString toRef str =
    str |> HQ.fromUrlString |> toRef


urlParser : (HashQualified -> Reference) -> Url.Parser.Parser (Reference -> a) a
urlParser toRef =
    Url.Parser.map toRef HQ.urlParser



-- HELPERS


equals : Reference -> Reference -> Bool
equals a b =
    case ( a, b ) of
        ( TermReference aHq, TermReference bHq ) ->
            HQ.equals aHq bHq

        ( TypeReference aHq, TypeReference bHq ) ->
            HQ.equals aHq bHq

        ( AbilityConstructorReference aHq, AbilityConstructorReference bHq ) ->
            HQ.equals aHq bHq

        ( DataConstructorReference aHq, DataConstructorReference bHq ) ->
            HQ.equals aHq bHq

        _ ->
            False


{-| Like `equals`, but compares deeper such that a HashQualified with the same
Hash as a HashOnly are considered the same
-}
same : Reference -> Reference -> Bool
same a b =
    case ( a, b ) of
        ( TermReference aHq, TermReference bHq ) ->
            HQ.same aHq bHq

        ( TypeReference aHq, TypeReference bHq ) ->
            HQ.same aHq bHq

        ( AbilityConstructorReference aHq, AbilityConstructorReference bHq ) ->
            HQ.same aHq bHq

        ( DataConstructorReference aHq, DataConstructorReference bHq ) ->
            HQ.same aHq bHq

        _ ->
            False


hashQualified : Reference -> HashQualified
hashQualified ref =
    case ref of
        TermReference hq ->
            hq

        TypeReference hq ->
            hq

        AbilityConstructorReference hq ->
            hq

        DataConstructorReference hq ->
            hq


fqn : Reference -> Maybe FQN
fqn =
    hashQualified >> HQ.name


hash : Reference -> Maybe Hash
hash =
    hashQualified >> HQ.hash



-- TRANSFORM


toString : Reference -> String
toString ref =
    case ref of
        TermReference hq ->
            "term__" ++ HQ.toString hq

        TypeReference hq ->
            "type__" ++ HQ.toString hq

        AbilityConstructorReference hq ->
            "ability_constructor__" ++ HQ.toString hq

        DataConstructorReference hq ->
            "data_constructor__" ++ HQ.toString hq


toHumanString : Reference -> String
toHumanString ref =
    case ref of
        TermReference hq ->
            "Term " ++ HQ.toString hq

        TypeReference hq ->
            "Type " ++ HQ.toString hq

        AbilityConstructorReference hq ->
            "Ability constructor " ++ HQ.toString hq

        DataConstructorReference hq ->
            "Data Constructor " ++ HQ.toString hq


toIcon : Reference -> Icon msg
toIcon ref =
    case ref of
        TermReference _ ->
            Icon.term

        TypeReference _ ->
            Icon.type_

        AbilityConstructorReference _ ->
            Icon.abilityConstructor

        DataConstructorReference _ ->
            Icon.dataConstructor


map : (HashQualified -> HashQualified) -> Reference -> Reference
map f ref =
    case ref of
        TermReference hq ->
            TermReference (f hq)

        TypeReference hq ->
            TypeReference (f hq)

        AbilityConstructorReference hq ->
            AbilityConstructorReference (f hq)

        DataConstructorReference hq ->
            DataConstructorReference (f hq)
