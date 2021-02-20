module App exposing (..)

import Api
import Definition exposing (Definition(..))
import FullyQualifiedName as FQN exposing (FQN, unqualifiedName)
import FullyQualifiedNameSet as FQNSet exposing (FQNSet)
import Hash exposing (Hash)
import Html exposing (Html, a, article, aside, button, code, div, h1, h2, h3, header, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import NamespaceListing exposing (DefinitionListing(..), NamespaceListing(..), NamespaceListingContent)
import OpenDefinitions exposing (OpenDefinitions)
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Icon as Icon



-- MODEL


type alias Model =
    { openDefinitions : OpenDefinitions
    , rootNamespaceListing : WebData NamespaceListing
    , expandedNamespaceListings : FQNSet
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { openDefinitions = OpenDefinitions.empty
            , rootNamespaceListing = Loading
            , expandedNamespaceListings = FQNSet.empty
            }
    in
    ( model, fetchRootNamespaceListing )



-- UPDATE


type Msg
    = ToggleExpandedNamespaceListing FQN
    | FetchSubNamespaceListingFinished FQN (Result Http.Error NamespaceListing)
    | FetchRootNamespaceListingFinished (Result Http.Error NamespaceListing)
    | OpenDefinition Hash
    | CloseDefinition Hash
    | FetchOpenDefinitionsFinished (List Hash) (Result Http.Error (List Definition))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleExpandedNamespaceListing fqn ->
            let
                shouldExpand =
                    not (FQNSet.member fqn model.expandedNamespaceListings)

                newModel =
                    -- TODO: Update to Loading
                    { model
                        | expandedNamespaceListings =
                            FQNSet.toggle fqn
                                model.expandedNamespaceListings
                    }

                cmd =
                    if shouldExpand then
                        fetchSubNamespaceListing fqn

                    else
                        Cmd.none
            in
            ( newModel, cmd )

        FetchSubNamespaceListingFinished fetchedFqn result ->
            let
                replaceNamespaceListing ((NamespaceListing hash fqn _) as namespaceListing) =
                    if FQN.equals fetchedFqn fqn then
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

        OpenDefinition hash ->
            ( { model
                | openDefinitions =
                    OpenDefinitions.insert hash
                        Loading
                        model.openDefinitions
              }
            , fetchDefinitions [ hash ]
            )

        CloseDefinition hash ->
            ( { model
                | openDefinitions =
                    OpenDefinitions.remove hash
                        model.openDefinitions
              }
            , Cmd.none
            )

        FetchOpenDefinitionsFinished hashes result ->
            let
                list =
                    case result of
                        Err err ->
                            List.map (\h -> ( h, Failure err )) hashes

                        Ok definitions ->
                            List.map (\d -> ( Definition.hash d, Success d )) definitions

                nextOpenDefinitions =
                    OpenDefinitions.insertList list model.openDefinitions
            in
            ( { model | openDefinitions = nextOpenDefinitions }, Cmd.none )



-- HTTP


fetchRootNamespaceListing : Cmd Msg
fetchRootNamespaceListing =
    let
        rootFqn =
            FQN.fromString "."
    in
    Http.get
        { url = Api.listUrl Nothing
        , expect = Http.expectJson FetchRootNamespaceListingFinished (NamespaceListing.decode rootFqn)
        }


fetchSubNamespaceListing : FQN -> Cmd Msg
fetchSubNamespaceListing fqn =
    Http.get
        { url = Api.listUrl (Just (FQN.toString fqn))
        , expect = Http.expectJson (FetchSubNamespaceListingFinished fqn) (NamespaceListing.decode fqn)
        }


fetchDefinitions : List Hash -> Cmd Msg
fetchDefinitions hashes =
    Http.get
        { url = Api.definitionUrl (List.map Hash.toString hashes)
        , expect = Http.expectJson (FetchOpenDefinitionsFinished hashes) Definition.decodeList
        }



-- VIEW


viewDefinitionListing : DefinitionListing -> Html Msg
viewDefinitionListing listing =
    case listing of
        TypeListing hash fqn ->
            a [ class "node type", onClick (OpenDefinition hash) ]
                [ Icon.view Icon.Type
                , label [] [ text (unqualifiedName fqn) ]
                ]

        TermListing hash fqn ->
            a [ class "node term", onClick (OpenDefinition hash) ]
                [ Icon.view Icon.Term
                , label [] [ text (unqualifiedName fqn) ]
                ]

        PatchListing _ ->
            a [ class "node patch" ]
                [ Icon.view Icon.Patch
                , label [] [ text "Patch" ]
                ]


viewLoadedNamespaceListingContent : FQNSet -> NamespaceListingContent -> Html Msg
viewLoadedNamespaceListingContent expandedNamespaceListings content =
    let
        namespaces =
            List.map (viewNamespaceListing expandedNamespaceListings) content.namespaces

        definitions =
            List.map viewDefinitionListing content.definitions
    in
    div [] (namespaces ++ definitions)


viewNamespaceListingContent : FQNSet -> WebData NamespaceListingContent -> Html Msg
viewNamespaceListingContent expandedNamespaceListings content =
    case content of
        Success loadedContent ->
            viewLoadedNamespaceListingContent expandedNamespaceListings loadedContent

        Failure err ->
            UI.errorMessage (Api.errorToString err)

        NotAsked ->
            UI.nothing

        Loading ->
            UI.loadingPlaceholder


viewNamespaceListing : FQNSet -> NamespaceListing -> Html Msg
viewNamespaceListing expandedNamespaceListings (NamespaceListing hash fqn content) =
    let
        ( caretIcon, namespaceContent ) =
            if FQNSet.member fqn expandedNamespaceListings then
                ( Icon.CaretDown
                , div [ class "namespace-content" ]
                    [ viewNamespaceListingContent
                        expandedNamespaceListings
                        content
                    ]
                )

            else
                ( Icon.CaretRight, UI.nothing )
    in
    div [ class "subtree" ]
        [ a
            [ class "node namespace"
            , onClick (ToggleExpandedNamespaceListing fqn)
            ]
            [ Icon.view caretIcon, label [] [ text (unqualifiedName fqn) ] ]
        , namespaceContent
        ]


viewAllNamespaces : FQNSet -> WebData NamespaceListing -> Html Msg
viewAllNamespaces expandedNamespaceListings namespaceRoot =
    let
        listings =
            case namespaceRoot of
                Success (NamespaceListing _ fqn content) ->
                    viewNamespaceListingContent expandedNamespaceListings content

                Failure err ->
                    UI.errorMessage (Api.errorToString err)

                NotAsked ->
                    UI.spinner

                Loading ->
                    UI.spinner
    in
    div [ id "all-namespaces" ]
        [ h2 [] [ text "All Namespaces" ]
        , div [ class "namespace-tree" ] [ listings ]
        ]


viewMainSidebar : Model -> Html Msg
viewMainSidebar model =
    aside
        [ id "main-sidebar" ]
        [ header [] [ h1 [] [ text "~/.unison" ] ]
        , viewAllNamespaces
            model.expandedNamespaceListings
            model.rootNamespaceListing
        ]


viewDefinition : Hash -> WebData Definition -> Html Msg
viewDefinition hash definition =
    case definition of
        Success def ->
            Definition.view (CloseDefinition hash) def

        Failure err ->
            Definition.viewError (CloseDefinition hash) err

        NotAsked ->
            UI.nothing

        Loading ->
            Definition.viewLoading


viewOpenDefinitions : OpenDefinitions -> List (Html Msg)
viewOpenDefinitions openDefinitions =
    openDefinitions
        |> OpenDefinitions.toList
        |> List.map (\( h, d ) -> viewDefinition h d)


viewWorkspace : Model -> Html Msg
viewWorkspace model =
    article [ id "workspace" ]
        [ header [ id "workspace-toolbar" ] []
        , section [ id "workspace-content" ]
            [ section
                [ class "definitions-pane" ]
                (viewOpenDefinitions model.openDefinitions)
            ]
        ]


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ viewMainSidebar model
        , viewWorkspace model
        ]
