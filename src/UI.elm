module UI exposing (..)

import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)


icon : String -> Html msg
icon name =
    i [ class ("fas fa-" ++ name) ] []


empty : Html msg
empty =
    text ""


spinner : Html msg
spinner =
    div [ class "spinner" ] [ text "Loading..." ]
