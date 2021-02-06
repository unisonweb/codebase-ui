module App exposing (..)

import Api
import Definition exposing (Definition)
import FullyQualifiedName exposing (unqualifiedName)
import Hash exposing (Hash(..))
import HashSet exposing (HashSet)
import Html exposing (Html, a, article, aside, button, div, header, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List.Nonempty
import NamespaceListing exposing (DefinitionListing(..), NamespaceListing(..), NamespaceListingContent)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import UI



-- MODEL


type alias OpenDefinition =
    ( Definition, Bool )


type alias Model =
    { query : String
    , openDefinitions : List OpenDefinition
    , rootNamespaceListing : WebData NamespaceListing
    , expandedNamespaceListings : HashSet
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { query = ""
            , openDefinitions = []
            , rootNamespaceListing = Loading
            , expandedNamespaceListings = HashSet.empty
            }
    in
    ( model, fetchRootNamespaceListing )



-- UPDATE


type Msg
    = UpdateQuery String
    | CloseDefinition Hash
    | ToggleShowCode Hash
    | ToggleExpandedNamespaceListing Hash
    | FetchSubNamespaceListingFinished Hash (Result Http.Error NamespaceListing)
    | FetchRootNamespaceListingFinished (Result Http.Error NamespaceListing)


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

        ToggleExpandedNamespaceListing hash ->
            let
                shouldExpand =
                    not (HashSet.member hash model.expandedNamespaceListings)

                newModel =
                    { model
                        | expandedNamespaceListings =
                            HashSet.toggle hash
                                model.expandedNamespaceListings
                    }

                cmd =
                    if shouldExpand then
                        fetchSubNamespaceListing hash

                    else
                        Cmd.none
            in
            ( newModel, cmd )

        FetchSubNamespaceListingFinished fetchedHash result ->
            let
                replaceNamespaceListing ((NamespaceListing hash fqn _) as namespaceListing) =
                    if Hash.equals fetchedHash hash then
                        case result of
                            Ok (NamespaceListing _ _ content) ->
                                NamespaceListing hash fqn content

                            Err err ->
                                NamespaceListing hash fqn (Failure err)

                    else
                        namespaceListing

                nextNamespaceListing =
                    RemoteData.map (NamespaceListing.map replaceNamespaceListing) model.rootNamespaceListing
            in
            ( { model | rootNamespaceListing = nextNamespaceListing }, Cmd.none )

        FetchRootNamespaceListingFinished result ->
            case result of
                Ok (NamespaceListing hash fqn content) ->
                    ( { model | rootNamespaceListing = Success (NamespaceListing hash fqn content) }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | rootNamespaceListing = Failure err }, Cmd.none )



-- Http


fetchRootNamespaceListing : Cmd Msg
fetchRootNamespaceListing =
    Http.get
        { url = Api.listUrl Nothing
        , expect = Http.expectJson FetchRootNamespaceListingFinished NamespaceListing.decode
        }


fetchSubNamespaceListing : Hash -> Cmd Msg
fetchSubNamespaceListing hash =
    Http.get
        { url = Api.listUrl (Just (Hash.toString hash))
        , expect = Http.expectJson (FetchSubNamespaceListingFinished hash) NamespaceListing.decode
        }



-- VIEW


viewDefinitionListing : DefinitionListing -> Html Msg
viewDefinitionListing definition =
    case definition of
        Type hash fqn ->
            a [ class "node type" ] [ text (unqualifiedName fqn) ]

        Term hash fqn ->
            a [ class "node term" ] [ text (unqualifiedName fqn) ]

        Patch _ ->
            a [ class "node patch" ] [ text "Patch" ]


viewLoadedNamespaceListingContent : HashSet -> NamespaceListingContent -> Html Msg
viewLoadedNamespaceListingContent expandedNamespaceListings content =
    let
        namespaces =
            List.map (viewNamespaceListing expandedNamespaceListings) content.namespaces

        definitions =
            List.map viewDefinitionListing content.definitions
    in
    div [] (namespaces ++ definitions)


viewNamespaceListingContent : HashSet -> WebData NamespaceListingContent -> Html Msg
viewNamespaceListingContent expandedNamespaceListings content =
    case content of
        Success loadedContent ->
            viewLoadedNamespaceListingContent expandedNamespaceListings loadedContent

        Failure err ->
            text (Api.errorToString err)

        NotAsked ->
            UI.nothing

        Loading ->
            UI.spinner


viewNamespaceListing : HashSet -> NamespaceListing -> Html Msg
viewNamespaceListing expandedNamespaceListings (NamespaceListing hash fqn content) =
    let
        namespaceContent =
            if HashSet.member hash expandedNamespaceListings then
                div [ class "namespace-content" ]
                    [ viewNamespaceListingContent
                        expandedNamespaceListings
                        content
                    ]

            else
                UI.nothing
    in
    div []
        [ a
            [ class "node namespace"
            , onClick (ToggleExpandedNamespaceListing hash)
            ]
            [ text (unqualifiedName fqn) ]
        , namespaceContent
        ]


viewAllNamespaces : HashSet -> WebData NamespaceListing -> Html Msg
viewAllNamespaces expandedNamespaceListings namespaceRoot =
    let
        listings =
            case namespaceRoot of
                Success (NamespaceListing _ fqn content) ->
                    viewNamespaceListingContent expandedNamespaceListings content

                Failure err ->
                    div [] [ text (Api.errorToString err) ]

                NotAsked ->
                    UI.spinner

                Loading ->
                    UI.spinner
    in
    div [ id "all-namespaces", class "namespace-tree" ]
        [ div [] [ text "All Namespaces" ]
        , listings
        ]


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
                , viewAllNamespaces model.expandedNamespaceListings model.rootNamespaceListing
                ]
            , section [ id "main-pane" ]
                [ header [ class "pane-header" ] []
                , div [] []
                ]
            ]
        ]
