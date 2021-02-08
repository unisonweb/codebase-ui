module UI.Icon exposing (..)

import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)


icon : String -> Html msg
icon name =
    i [ class ("icon fas fa-" ++ name) ] []


caretRight : Html msg
caretRight =
    icon "caret-right"


caretDown : Html msg
caretDown =
    icon "caret-down"
