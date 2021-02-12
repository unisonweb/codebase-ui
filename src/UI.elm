module UI exposing (..)

import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)


nothing : Html msg
nothing =
    text ""


spinner : Html msg
spinner =
    div [ class "spinner" ] [ text "Loading..." ]


type Sizing
    = Base
    | Medium


loadingPlaceholder : Html msg
loadingPlaceholder =
    div [ class "loading-placeholder" ] []


errorMessage : String -> Html msg
errorMessage message =
    div [ class "error-message" ] [ text message ]
