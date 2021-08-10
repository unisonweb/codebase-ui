module UI.Tooltip exposing (Arrow(..), Tooltip, tooltip, view, withArrow)

import Html exposing (Html, div, span)
import Html.Attributes exposing (class)


type Arrow
    = None
    | Top
    | TopLeft
    | TopRight
    | Right
    | RightTop
    | RightBottom
    | Bottom
    | BottomLeft
    | BottomRight
    | Left
    | LeftTop
    | LeftBottom


type alias Tooltip msg =
    { arrow : Arrow
    , trigger : Html msg
    , content : Html msg
    }


tooltip : Html msg -> Html msg -> Tooltip msg
tooltip trigger content =
    { arrow = Top, trigger = trigger, content = content }


withArrow : Arrow -> Tooltip msg -> Tooltip msg
withArrow arrow tooltip_ =
    { tooltip_ | arrow = arrow }


view : Tooltip msg -> Html msg
view { arrow, content, trigger } =
    let
        tooltip_ =
            div [ class "tooltip", class (arrowToClass arrow) ] [ content ]
    in
    span [ class "tooltip-trigger" ] [ tooltip_, trigger ]



-- INTERNAL


arrowToClass : Arrow -> String
arrowToClass arrow =
    case arrow of
        None ->
            "no-arrow"

        Top ->
            "top"

        TopLeft ->
            "top-left"

        TopRight ->
            "top-right"

        Right ->
            "right"

        RightTop ->
            "right-top"

        RightBottom ->
            "right-bottom"

        Bottom ->
            "bottom"

        BottomLeft ->
            "bottom-left"

        BottomRight ->
            "bottom-right"

        Left ->
            "left"

        LeftTop ->
            "left-top"

        LeftBottom ->
            "left-bottom"
