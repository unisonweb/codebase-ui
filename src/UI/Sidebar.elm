module UI.Sidebar exposing (..)

import Html exposing (Html, a, aside, h3, label, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


section : String -> List (Html msg) -> Html msg
section label content =
    Html.section [ class "sidebar-section" ]
        (header label :: content)


header : String -> Html msg
header label =
    h3 [ class "sidebar-header" ] [ text label ]


item : msg -> String -> Html msg
item clickMsg label_ =
    a
        [ class "sidebar-item"
        , onClick clickMsg
        ]
        [ label [] [ text label_ ]
        ]


view : List (Html msg) -> Html msg
view content =
    aside [ id "main-sidebar" ] content
