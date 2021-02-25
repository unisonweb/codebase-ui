module UI exposing (..)

import Html exposing (Html, code, div, pre, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


button : String -> msg -> Html msg
button label clickMsg =
    Html.button [ onClick clickMsg ] [ text label ]


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


codeInline : Html msg -> Html msg
codeInline content =
    code [] [ content ]


codeBlock : Html msg -> Html msg
codeBlock content =
    pre [] [ code [] [ content ] ]
