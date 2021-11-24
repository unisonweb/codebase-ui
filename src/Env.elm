module Env exposing (..)

import Api exposing (ApiBasePath(..))
import Env.AppContext as AppContext exposing (AppContext)
import Perspective exposing (Perspective)


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
    , appContext = AppContext.fromString flags.appContext
    , perspective = perspective
    }


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
