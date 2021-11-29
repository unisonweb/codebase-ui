module Env.AppContext exposing (..)


type AppContext
    = UnisonShare
    | UnisonLocal


fromString : String -> AppContext
fromString rawContext =
    if rawContext == "UnisonShare" then
        UnisonShare

    else
        UnisonLocal


toString : AppContext -> String
toString appContext =
    case appContext of
        UnisonShare ->
            "Unison Share"

        UnisonLocal ->
            "Unison Local"


isUnisonShare : AppContext -> Bool
isUnisonShare appContext =
    case appContext of
        UnisonShare ->
            True

        UnisonLocal ->
            False


isUnisonLocal : AppContext -> Bool
isUnisonLocal appContext =
    case appContext of
        UnisonShare ->
            False

        UnisonLocal ->
            True
