module Main exposing (..)

import Browser
import Html exposing (Html, a, article, aside, button, div, header, i, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { query : String
    }


init : Model
init =
    { query = "" }



-- UPDATE


type Msg
    = UpdateQuery String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateQuery query ->
            { model | query = query }



-- VIEW


icon : String -> Html msg
icon name =
    i [ class ("fas fa-" ++ name) ] []


viewDefinition : Html msg
viewDefinition =
    div
        [ class "definition" ]
        [ a [ class "action" ] [ icon "caret-right" ]
        , div [ class "definition-summary" ]
            [ label [ class "definition-namespace-path" ]
                [ a [ class "namespace" ] [ text "base" ]
                , span [ class "slash" ] [ text "/" ]
                , a [ class "namespace" ] [ text "v1" ]
                , span [ class "slash" ] [ text "/" ]
                , a [ class "namespace" ] [ text "List" ]
                ]
            , label
                []
                [ text "flatMap" ]
            ]
        , a [ class "action" ] [ icon "times" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ header [ id "main-header" ] [ text "Unison" ]
        , article [ id "panes" ]
            [ section [ id "main-nav-pane" ]
                [ header [ id "definition-search", class "pane-header" ]
                    [ icon "search"
                    , input
                        [ type_ "text"
                        , placeholder "Namespace, name, or signature"
                        , value model.query
                        , onInput UpdateQuery
                        ]
                        []
                    ]
                ]
            , section [ id "main-pane" ]
                [ header [ class "pane-header" ] []
                , viewDefinition
                , viewDefinition
                , viewDefinition
                , viewDefinition
                ]
            ]
        ]
