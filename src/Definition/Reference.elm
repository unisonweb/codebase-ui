module Definition.Reference exposing (..)

import FullyQualifiedName exposing (FQN)
import HashQualified as HQ exposing (HashQualified)
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
