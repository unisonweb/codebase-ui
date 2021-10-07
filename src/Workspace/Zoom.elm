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
            Near

        Near ->
            Far


cycleEdges : Zoom -> Zoom
cycleEdges z =
    case z of
        Far ->
            Near

        Medium ->
            Far

        Near ->
            Far
