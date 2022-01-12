module UI exposing (..)

import Html exposing (Attribute, Html, code, div, hr, pre, span, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import UI.Icon as Icon


codeBlock : List (Attribute msg) -> Html msg -> Html msg
codeBlock attrs code_ =
    pre attrs [ code [] [ code_ ] ]


bold : String -> Html msg
bold text_ =
    strong [] [ text text_ ]


inlineCode : List (Attribute msg) -> Html msg -> Html msg
inlineCode attrs code_ =
    code (class "inline-code" :: attrs) [ code_ ]


nothing : Html msg
nothing =
    text ""


badge : Html msg -> Html msg
badge content =
    span [ class "badge" ] [ content ]


optionBadge : msg -> Html msg -> Html msg
optionBadge removeMsg content =
    span [ class "option-badge", onClick removeMsg ] [ Icon.view Icon.x, content ]


subtle : String -> Html msg
subtle label =
    span [ class "subtle" ] [ text label ]


loadingPlaceholder : Html msg
loadingPlaceholder =
    div [ class "loading-placeholder" ] []


loadingPlaceholderRow : Html msg
loadingPlaceholderRow =
    div [ class "loading-placeholder-row" ]
        [ div [ class "loading-placeholder" ] []
        ]


errorMessage : String -> Html msg
errorMessage message =
    div [ class "error-message" ] [ text message ]


emptyStateMessage : String -> Html msg
emptyStateMessage message =
    div [ class "empty-state" ] [ text message ]


divider : Html msg
divider =
    hr [ class "divider" ] []


charWidth : Int -> String
charWidth numChars =
    String.fromInt numChars ++ "ch"
