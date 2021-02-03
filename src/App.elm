module App exposing (..)

import Api
import Definition exposing (Definition)
import Html exposing (Html, a, article, aside, button, div, header, i, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List.Nonempty
import Namespace
import RemoteData
import UnisonHash exposing (UnisonHash)



-- MODEL


type alias OpenDefinition =
    ( Definition, Bool )


type alias Model =
    { query : String
    , openDefinitions : List OpenDefinition
    , namespaces : RemoteData.WebData Namespace.Namespace
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { query = ""
            , openDefinitions = []
            , namespaces = RemoteData.Loading
            }
    in
    ( model, fetchNamespaces )



-- UPDATE


type Msg
    = UpdateQuery String
    | CloseDefinition UnisonHash
    | ToggleShowCode UnisonHash
    | FetchNamespace
    | FetchNamespaceFinished (Result Http.Error Namespace.Namespace)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        CloseDefinition hash ->
            let
                nextOpenDefinitions =
                    model.openDefinitions
            in
            ( { model | openDefinitions = nextOpenDefinitions }, Cmd.none )

        ToggleShowCode hash ->
            let
                nextOpenDefinitions =
                    model.openDefinitions
            in
            ( { model | openDefinitions = nextOpenDefinitions }, Cmd.none )

        FetchNamespace ->
            ( model, Cmd.none )

        FetchNamespaceFinished result ->
            case result of
                Ok namespace ->
                    ( { model | namespaces = RemoteData.Success namespace }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | namespaces = RemoteData.Failure err }, Cmd.none )



-- Http


fetchNamespaces : Cmd Msg
fetchNamespaces =
    Http.get
        { url = Api.listUrl
        , expect = Http.expectJson FetchNamespaceFinished Namespace.decode
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


icon : String -> Html msg
icon name =
    i [ class ("fas fa-" ++ name) ] []


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody text ->
            "Unexpected response from api: " ++ text

        Http.BadUrl url ->
            "Malformed url: " ++ url


viewNamespaceChild : Namespace.NamespaceChild -> Html Msg
viewNamespaceChild namespaceChild =
    case namespaceChild of
        Namespace.SubNamespace name size ->
            div [] [ text name ]

        Namespace.Type name hash ->
            div [] [ text name ]


viewNamespaceTree : RemoteData.WebData Namespace.Namespace -> Html Msg
viewNamespaceTree namespace =
    let
        content =
            case namespace of
                RemoteData.Success ns ->
                    List.map viewNamespaceChild ns.children

                RemoteData.Failure err ->
                    [ text (errorToString err) ]

                RemoteData.NotAsked ->
                    [ text "Loading" ]

                RemoteData.Loading ->
                    [ text "Loading" ]
    in
    div [ class "namespace-tree" ] content


viewNamespacePath : Namespace.Path -> Html msg
viewNamespacePath (Namespace.Path path) =
    let
        namespaceLinks =
            path
                |> List.Nonempty.map (\p -> a [ class "namespace" ] [ text p ])
                |> List.Nonempty.toList

        slash =
            span [ class "slash" ] [ text "/" ]
    in
    label
        [ class "definition-namespace-path" ]
        (List.intersperse slash namespaceLinks)


viewNothing : Html msg
viewNothing =
    text ""


viewDefinition : OpenDefinition -> Html msg
viewDefinition _ =
    div [] []


view : Model -> Html Msg
view model =
    div []
        [ header [ id "main-header" ] [ text "Unison Codebase browser" ]
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
                , viewNamespaceTree model.namespaces
                ]
            , section [ id "main-pane" ]
                [ header [ class "pane-header" ] []
                , div [] (List.map viewDefinition model.openDefinitions)
                ]
            ]
        ]
