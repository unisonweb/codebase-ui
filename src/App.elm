module App exposing (..)

import Api
import Definition exposing (Definition)
import Hash exposing (Hash)
import Html exposing (Html, a, article, aside, button, div, header, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List.Nonempty
import NamespaceTree exposing (NamespaceTree(..))
import RemoteData exposing (RemoteData(..), WebData)
import UI



-- MODEL


type alias OpenDefinition =
    ( Definition, Bool )


type alias Model =
    { query : String
    , openDefinitions : List OpenDefinition
    , namespaceTree : WebData NamespaceTree
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { query = ""
            , openDefinitions = []
            , namespaceTree = Loading
            }
    in
    ( model, fetchNamespaceTree )



-- UPDATE


type Msg
    = UpdateQuery String
    | CloseDefinition Hash
    | ToggleShowCode Hash
    | FetchNamespaceTree
    | FetchNamespaceTreeFinished (Result Http.Error NamespaceTree)


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

        FetchNamespaceTree ->
            ( model, fetchNamespaceTree )

        FetchNamespaceTreeFinished result ->
            case result of
                Ok tree ->
                    ( { model | namespaceTree = Success tree }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | namespaceTree = Failure err }, Cmd.none )



-- Http


fetchNamespaceTree : Cmd Msg
fetchNamespaceTree =
    Http.get
        { url = Api.listUrl
        , expect = Http.expectJson FetchNamespaceTreeFinished NamespaceTree.decode
        }



-- VIEW


viewNamespaceTree : NamespaceTree -> Html Msg
viewNamespaceTree tree =
    case tree of
        NamespaceTree.Namespace name subTree ->
            div [] [ text name ]

        NamespaceTree.Type name ->
            div [] [ text name ]

        NamespaceTree.Term name ->
            div [] [ text name ]


viewNamespaceTrees : WebData (List NamespaceTree) -> Html Msg
viewNamespaceTrees treeRequest =
    case treeRequest of
        Success trees ->
            div [] (List.map viewNamespaceTree trees)

        Failure err ->
            div [] [ text (Api.errorToString err) ]

        NotAsked ->
            UI.spinner

        Loading ->
            UI.spinner


viewAllNamespaces : WebData NamespaceTree -> Html Msg
viewAllNamespaces namespaceRoot =
    let
        content =
            case namespaceRoot of
                Success root ->
                    case root of
                        NamespaceTree.Namespace name subTrees ->
                            viewNamespaceTrees subTrees

                        _ ->
                            div [] [ text "TODO, Fix types such that this branch cant be constructed" ]

                Failure err ->
                    div [] [ text (Api.errorToString err) ]

                NotAsked ->
                    UI.spinner

                Loading ->
                    UI.spinner
    in
    div [ id "all-namespaces", class "namespace-tree" ] [ content ]


view : Model -> Html Msg
view model =
    div []
        [ article [ id "panes" ]
            [ section [ id "main-nav-pane" ]
                [ header [ id "definition-search", class "pane-header" ]
                    [ UI.icon "search"
                    , input
                        [ type_ "text"
                        , placeholder "Namespace, name, or signature"
                        , value model.query
                        , onInput UpdateQuery
                        ]
                        []
                    ]
                , viewAllNamespaces model.namespaceTree
                ]
            , section [ id "main-pane" ]
                [ header [ class "pane-header" ] []
                , div [] []
                ]
            ]
        ]
