module Workspace.Reference exposing (..)

import HashQualified as HQ exposing (HashQualified)
import Url.Parser


type Reference
    = TermReference HashQualified
    | TypeReference HashQualified



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



-- TRANSFORM


toString : Reference -> String
toString ref =
    case ref of
        TermReference hq ->
            "term/" ++ HQ.toString hq

        TypeReference hq ->
            "type/" ++ HQ.toString hq
