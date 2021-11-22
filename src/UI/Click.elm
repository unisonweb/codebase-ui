module UI.Click exposing (..)

import Html exposing (Attribute, Html, a)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)


type Click msg
    = ExternalHref String
    | OnClick msg


attrs : Click msg -> List (Attribute msg)
attrs click =
    case click of
        ExternalHref href_ ->
            [ href href_, rel "noopener", target "_blank" ]

        OnClick msg ->
            [ onClick msg ]


view : List (Attribute msg) -> List (Html msg) -> Click msg -> Html msg
view attrs_ content click =
    a (attrs_ ++ attrs click) content
