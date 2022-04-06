module Lib.OperatingSystem exposing (..)


type OperatingSystem
    = MacOS
    | Windows
    | Linux
    | Android
    | IOS
    | Unknown


fromString : String -> OperatingSystem
fromString rawOs =
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
