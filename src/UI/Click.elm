module UI.Click exposing (..)

import Html exposing (Attribute, Html, a)
import Html.Attributes exposing (href, rel, target)
import Html.Events exposing (onClick)


type Click msg
    = ExternalHref String
    | OnClick msg


view : List (Attribute msg) -> List (Html msg) -> Click msg -> Html msg
view attrs content click =
    case click of
        ExternalHref href_ ->
            a (attrs ++ [ href href_, rel "noopener", target "_blank" ]) content

        OnClick msg ->
            a (attrs ++ [ onClick msg ]) content
