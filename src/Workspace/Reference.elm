module Workspace.Reference exposing (..)

import HashQualified exposing (HashQualified)


type Reference
    = TermReference HashQualified
    | TypeReference HashQualified


fromString : (HashQualified -> Reference) -> String -> Reference
fromString toRef str =
    str |> HashQualified.fromString |> toRef


fromUrlString : (HashQualified -> Reference) -> String -> Reference
fromUrlString toRef str =
    str |> HashQualified.fromUrlString |> toRef


toString : Reference -> String
toString ref =
    case ref of
        TermReference hq ->
            "term/" ++ HashQualified.toString hq

        TypeReference hq ->
            "type/" ++ HashQualified.toString hq


hashQualified : Reference -> HashQualified
hashQualified ref =
    case ref of
        TermReference hq ->
            hq

        TypeReference hq ->
            hq
