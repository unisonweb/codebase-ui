module UI.Button exposing (ButtonType(..), callToAction, primary, secondary, view)

import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type ButtonType
    = CallToAction
    | Primary
    | Secondary


callToAction : msg -> String -> Html msg
callToAction =
    view CallToAction


primary : msg -> String -> Html msg
primary =
    view Primary


secondary : msg -> String -> Html msg
secondary =
    view Secondary


view : ButtonType -> msg -> String -> Html msg
view type_ clickMsg label =
    button [ class (buttonTypeToClassName type_), onClick clickMsg ]
        [ text label
        ]



-- INTERNAL


buttonTypeToClassName : ButtonType -> String
buttonTypeToClassName type_ =
    case type_ of
        CallToAction ->
            "call-to-action"

        Primary ->
            "primary"

        Secondary ->
            "secondary"
