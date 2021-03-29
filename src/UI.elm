module UI exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)


nothing : Html msg
nothing =
    text ""


spinner : Html msg
spinner =
    div [ class "spinner" ] [ text "Loading..." ]


loadingPlaceholder : Html msg
loadingPlaceholder =
    div [ class "loading-placeholder" ] []


errorMessage : String -> Html msg
errorMessage message =
    div [ class "error-message" ] [ text message ]


emptyStateMessage : String -> Html msg
emptyStateMessage message =
    div [ class "empty-state" ] [ text message ]


charWidth : Int -> String
charWidth numChars =
    String.fromInt numChars ++ "ch"


tooltip : Html msg -> Html msg
tooltip content =
    div [ class "tooltip" ] [ content ]


withTooltip : Html msg -> Html msg -> Html msg
withTooltip content triggerContent =
    span [ class "with-tooltip" ] [ tooltip content, triggerContent ]
