module Env exposing (..)

import Api exposing (ApiBasePath(..))


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
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiBasePath : List String
    }


fromFlags : Flags -> Env
fromFlags flags =
    Env (operatingSystemFromString flags.operatingSystem) flags.basePath (ApiBasePath flags.apiBasePath)


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
