module Env exposing (..)

import Api exposing (ApiBasePath(..))
import Perspective exposing (Perspective)


type AppContext
    = UnisonShare
    | Ucm


type OperatingSystem
    = MacOS
    | Windows
    | Linux
    | Android
    | IOS
    | Unknown


type alias Env =
    { operatingSystem : OperatingSystem
    , basePath : String
    , apiBasePath : ApiBasePath
    , appContext : AppContext
    , perspective : Perspective
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiBasePath : List String
    , appContext : String
    }


init : Flags -> Perspective -> Env
init flags perspective =
    { operatingSystem = operatingSystemFromString flags.operatingSystem
    , basePath = flags.basePath
    , apiBasePath = ApiBasePath flags.apiBasePath
    , appContext = appContextFromString flags.appContext
    , perspective = perspective
    }


appContextFromString : String -> AppContext
appContextFromString rawContext =
    if rawContext == "UnisonShare" then
        UnisonShare

    else
        Ucm


appContextToString : AppContext -> String
appContextToString appContext =
    case appContext of
        UnisonShare ->
            "Unison Share"

        Ucm ->
            "Unison Local"


isUnisonShare : AppContext -> Bool
isUnisonShare appContext =
    case appContext of
        UnisonShare ->
            True

        Ucm ->
            False


isUnisonLocal : AppContext -> Bool
isUnisonLocal appContext =
    case appContext of
        UnisonShare ->
            False

        Ucm ->
            True


operatingSystemFromString : String -> OperatingSystem
operatingSystemFromString rawOs =
    case rawOs of
        "macOS" ->
            MacOS

        "iOS" ->
            IOS

        "Windows" ->
            Windows

        "Android" ->
            Android

        "Linux" ->
            Linux

        _ ->
            Unknown
