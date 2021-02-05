module App exposing (..)

import Api
import Definition exposing (Definition)
import FullyQualifiedName exposing (FQN, unqualifiedName)
import Hash exposing (Hash)
import Html exposing (Html, a, article, aside, button, div, header, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List.Nonempty
import NamespaceTree exposing (NamespaceTree(..))
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import UI



-- MODEL


type alias OpenDefinition =
    ( Definition, Bool )


type alias Model =
    { query : String
    , openDefinitions : List OpenDefinition
    , namespaceTree : WebData NamespaceTree
    , expandedNamespaces : Set FQN
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { query = ""
            , openDefinitions = []
            , namespaceTree = Loading
            , expandedNamespaces = Set.empty
            }
    in
    ( model, fetchNamespaceTree )



-- UPDATE


type Msg
    = UpdateQuery String
    | CloseDefinition Hash
    | ToggleShowCode Hash
    | ToggleExpandedNamespaceTree FQN
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

        ToggleExpandedNamespaceTree fqn ->
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


viewNamespaceTree : Set FQN -> NamespaceTree -> Html Msg
viewNamespaceTree expandedNamespaces tree =
    case tree of
        NamespaceTree.Namespace fqn subTree ->
            a
                [ class "node namespace"
                , onClick (ToggleExpandedNamespaceTree fqn)
                ]
                [ text (unqualifiedName fqn) ]

        NamespaceTree.Type fqn ->
            a [ class "node type" ] [ text (unqualifiedName fqn) ]

        NamespaceTree.Term fqn ->
            a [ class "node term" ] [ text (unqualifiedName fqn) ]


viewNamespaceTrees : Set FQN -> WebData (List NamespaceTree) -> Html Msg
viewNamespaceTrees expandedNamespaces treeRequest =
    case treeRequest of
        Success trees ->
            div [] (List.map (viewNamespaceTree expandedNamespaces) trees)

        Failure err ->
            div [] [ text (Api.errorToString err) ]

        NotAsked ->
            UI.spinner

        Loading ->
            UI.spinner


viewAllNamespaces : Set FQN -> WebData NamespaceTree -> Html Msg
viewAllNamespaces expandedNamespaces namespaceRoot =
    let
        content =
            case namespaceRoot of
                Success root ->
                    case root of
                        NamespaceTree.Namespace name subTrees ->
                            viewNamespaceTrees expandedNamespaces subTrees

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
                , viewAllNamespaces model.expandedNamespaces model.namespaceTree
                ]
            , section [ id "main-pane" ]
                [ header [ class "pane-header" ] []
                , div [] []
                ]
            ]
        ]
