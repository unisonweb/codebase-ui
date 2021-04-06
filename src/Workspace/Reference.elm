module Workspace.Reference exposing (..)

import HashQualified exposing (HashQualified)


type Reference
    = TermReference HashQualified
    | TypeReference HashQualified


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
