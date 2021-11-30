module UI.Sidebar exposing (..)

import Html exposing (Attribute, Html, a, aside, div, h3, label, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


header : List (Html msg) -> Html msg
header content =
    Html.header [ class "sidebar-header" ] content


headerItem : List (Attribute msg) -> List (Html msg) -> Html msg
headerItem attrs content =
    div (attrs ++ [ class "sidebar-header-item" ]) content


section : String -> List (Html msg) -> Html msg
section label content =
    Html.section [ class "sidebar-section" ]
        (sectionTitle label :: content)


sectionTitle : String -> Html msg
sectionTitle label =
    h3 [ class "sidebar-section-title" ] [ text label ]


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
