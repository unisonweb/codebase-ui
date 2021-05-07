-- This module pairs with src/system.js


module System exposing (..)


type OperatingSystem
    = MacOS
    | Windows
    | Linux
    | Android
    | IOS
    | Unknown


type alias System =
    { operatingSystem : OperatingSystem }


fromRecord : { operatingSystem : String } -> System
fromRecord { operatingSystem } =
    System (operatingSystemFromString operatingSystem)


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
