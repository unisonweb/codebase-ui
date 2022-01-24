module PerspectiveLanding exposing (..)

import Api
import Definition.Doc as Doc
import Definition.Readme as Readme
import Definition.Reference exposing (Reference)
import Env exposing (Env)
import Env.AppContext exposing (AppContext(..))
import FullyQualifiedName as FQN exposing (FQN)
import Html exposing (Html, a, article, div, h2, header, p, section, span, strong, text)
import Html.Attributes exposing (class, href, id, rel, target)
import Namespace exposing (Namespace(..))
import Perspective
import RemoteData exposing (RemoteData(..))
import UI
import UI.Button as Button exposing (Button)
import UI.Icon as Icon


type alias Model =
    Doc.DocFoldToggles


init : Model
init =
    Doc.emptyDocFoldToggles


type Msg
    = OpenReference Reference
    | ToggleDocFold Doc.FoldId
    | Find


type OutMsg
    = OpenDefinition Reference
    | ShowFinderRequest
    | None


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        OpenReference r ->
            ( model, OpenDefinition r )

        Find ->
            ( model, ShowFinderRequest )

        ToggleDocFold fid ->
            ( Doc.toggleFold model fid, None )



-- VIEW


container : List (Html msg) -> Html msg
container content =
    article [ id "perspective-landing" ]
        [ section
            [ id "perspective-landing-content" ]
            content
        ]


viewLoading : Html msg
viewLoading =
    container
        [ div
            [ class "loading" ]
            [ UI.loadingPlaceholderRow
            , UI.loadingPlaceholderRow
            ]
        ]


viewError : FQN -> String -> Html msg
viewError fqn message =
    container
        [ div
            [ class "perspective-landing-error" ]
            [ header [] [ Icon.view Icon.warn, text ("Error loading " ++ FQN.toString fqn) ]
            , p [] [ text message ]
            ]
        ]


viewEmptyState : Html msg -> List (Html msg) -> Button msg -> Html msg
viewEmptyState title description cta =
    let
        fauxItem =
            div [ class "faux-empty-state-item" ]
                [ UI.loadingPlaceholderRow
                , UI.loadingPlaceholderRow
                ]
    in
    container
        [ section [ class "perspective-landing-empty-state" ]
            [ section
                [ class "content" ]
                (h2 [] [ title ]
                    :: description
                    ++ [ fauxItem
                       , fauxItem
                       , section [ class "actions" ] [ Button.view cta ]
                       ]
                )
            ]
        ]


viewEmptyStateCodebase : AppContext -> Html Msg
viewEmptyStateCodebase appContext =
    let
        button =
            Button.iconThenLabel Find Icon.search "Find Definitions"
                |> Button.primaryMono
                |> Button.medium
    in
    case appContext of
        UnisonLocal ->
            viewEmptyState
                (span [ class "unison-local" ] [ text "Your ", span [ class "context" ] [ text "Local" ], text " Unison Codebase" ])
                [ p [] [ text "Browse, search, read docs, open definitions, and explore your local codebase." ]
                , p []
                    [ text "Check out "
                    , a [ class "unison-share", href "https://share.unison-lang.org", rel "noopener", target "_blank" ] [ strong [] [ text "Unison Share" ] ]
                    , text " for community projects."
                    ]
                ]
                button

        UnisonShare ->
            viewEmptyState
                (span [ class "unison-share" ] [ text "Unison ", span [ class "context" ] [ text "Share" ] ])
                [ p [] [ text "Explore to discover and share Unison libraries, documentation, types, and terms." ] ]
                button


viewEmptyStateNamespace : FQN -> Html Msg
viewEmptyStateNamespace fqn =
    let
        fqn_ =
            FQN.toString fqn
    in
    viewEmptyState
        (FQN.view fqn)
        [ p [] [ text "Browse, search, read docs, open definitions, and explore" ] ]
        (Button.iconThenLabel Find Icon.search ("Find Definitions in " ++ fqn_)
            |> Button.primaryMono
            |> Button.medium
        )


view : Env -> Model -> Html Msg
view env model =
    case env.perspective of
        Perspective.Codebase _ ->
            viewEmptyStateCodebase env.appContext

        Perspective.Namespace { fqn, details } ->
            case details of
                NotAsked ->
                    viewLoading

                Loading ->
                    viewLoading

                Success (Namespace _ _ { readme }) ->
                    case readme of
                        Just r ->
                            container
                                [ div [ class "perspective-landing-readme" ]
                                    [ header [] [ Icon.view Icon.doc, text "README" ]
                                    , Readme.view OpenReference ToggleDocFold model r
                                    ]
                                ]

                        Nothing ->
                            viewEmptyStateNamespace fqn

                Failure error ->
                    viewError fqn (Api.errorToString error)
