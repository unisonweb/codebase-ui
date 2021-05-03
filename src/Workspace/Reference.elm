module Workspace.Reference exposing (..)

import HashQualified as HQ exposing (HashQualified)
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
