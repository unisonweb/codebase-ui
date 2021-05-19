module UI exposing (..)

import Html exposing (Attribute, Html, code, div, pre, span, text)
import Html.Attributes exposing (class)


codeBlock : List (Attribute msg) -> Html msg -> Html msg
codeBlock attrs code_ =
    pre attrs [ code [] [ code_ ] ]


nothing : Html msg
nothing =
    text ""


badge : Html msg -> Html msg
badge content =
    span [ class "badge" ] [ content ]


spinner : Html msg
spinner =
    div [ class "spinner" ] []


subtle : String -> Html msg
subtle label =
    span [ class "subtle" ] [ text label ]


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
