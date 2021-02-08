module UI exposing (..)

import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)


nothing : Html msg
nothing =
    text ""


spinner : Html msg
spinner =
    div [ class "spinner" ] [ text "Loading..." ]
