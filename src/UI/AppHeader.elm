module UI.AppHeader exposing (..)

import Html exposing (Html, a, header, section)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import UI
import UI.Banner as Banner exposing (Banner)
import UI.Button as Button exposing (Button)
import UI.Click as Click exposing (Click)
import UI.Icon as Icon


type AppTitle msg
    = AppTitle (Click msg) (Html msg)


type alias MenuToggle msg =
    { onClick : msg }


type alias AppHeader msg =
    { menuToggle : Maybe msg
    , appTitle : AppTitle msg
    , banner : Maybe (Banner msg)
    , rightButton : Maybe (Button msg)
    }


appHeader : AppTitle msg -> AppHeader msg
appHeader appTitle =
    { menuToggle = Nothing
    , appTitle = appTitle
    , banner = Nothing
    , rightButton = Nothing
    }


view : AppHeader msg -> Html msg
view appHeader_ =
    let
        menuToggle =
            case appHeader_.menuToggle of
                Nothing ->
                    UI.nothing

                Just toggle ->
                    a
                        [ class "menu-toggle", onClick toggle ]
                        [ Icon.view Icon.list ]

        banner =
            case appHeader_.banner of
                Nothing ->
                    UI.nothing

                Just banner_ ->
                    Banner.view banner_

        rightButton =
            appHeader_.rightButton
                |> Maybe.map Button.small
                |> Maybe.map Button.view
                |> Maybe.withDefault UI.nothing
    in
    view_
        [ menuToggle
        , viewAppTitle appHeader_.appTitle
        , section [ class "right" ] [ banner, rightButton ]
        ]


viewAppTitle : AppTitle msg -> Html msg
viewAppTitle (AppTitle click content) =
    Click.view [ class "app-title" ] [ content ] click


view_ : List (Html msg) -> Html msg
view_ content =
    header [ id "app-header" ] content
