module Env exposing (..)

import Api exposing (ApiBasePath(..))
import Browser.Navigation as Nav
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
    , navKey : Nav.Key
    , perspective : Perspective
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiBasePath : List String
    , appContext : String
    }


init : Flags -> Nav.Key -> Perspective -> Env
init flags navKey perspective =
    { operatingSystem = operatingSystemFromString flags.operatingSystem
    , basePath = flags.basePath
    , apiBasePath = ApiBasePath flags.apiBasePath
    , appContext = AppContext.fromString flags.appContext
    , navKey = navKey
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
