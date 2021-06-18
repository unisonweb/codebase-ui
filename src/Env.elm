module Env exposing (..)

import Api exposing (ApiBasePath(..))


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
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiBasePath : List String
    , appContext : String
    }


fromFlags : Flags -> Env
fromFlags flags =
    { operatingSystem = operatingSystemFromString flags.operatingSystem
    , basePath = flags.basePath
    , apiBasePath = ApiBasePath flags.apiBasePath
    , appContext = appContextFromString flags.appContext
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
