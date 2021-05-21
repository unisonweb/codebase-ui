module Workspace.Zoom exposing (..)


type Zoom
    = Far
    | Medium
    | Near


cycle : Zoom -> Zoom
cycle z =
    case z of
        Far ->
            Medium

        Medium ->
            -- TODO: Support source zoom
            Far

        Near ->
            Far
