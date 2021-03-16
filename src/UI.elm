module UI exposing (..)

import Html exposing (Html, code, div, pre, text)
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


codeInline : Html msg -> Html msg
codeInline content =
    code [] [ content ]


codeBlock : Html msg -> Html msg
codeBlock content =
    pre [] [ code [] [ content ] ]


charWidth : Int -> String
charWidth numChars =
    String.fromInt numChars ++ "ch"
