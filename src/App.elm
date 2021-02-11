module App exposing (..)

import Api
import Definition exposing (Definition)
import FullyQualifiedName exposing (unqualifiedName)
import Hash exposing (Hash)
import HashSet exposing (HashSet)
import Html exposing (Html, a, article, aside, button, div, h1, h2, header, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import NamespaceListing exposing (DefinitionListing(..), NamespaceListing(..), NamespaceListingContent)
import OpenDefinitions exposing (OpenDefinitions)
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Icon



-- MODEL


type alias Model =
    { openDefinitions : OpenDefinitions
    , rootNamespaceListing : WebData NamespaceListing
    , expandedNamespaceListings : HashSet
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { openDefinitions = OpenDefinitions.empty
            , rootNamespaceListing = Loading
            , expandedNamespaceListings = HashSet.empty
            }
    in
    ( model, fetchRootNamespaceListing )



-- UPDATE


type Msg
    = ToggleExpandedNamespaceListing Hash
    | FetchSubNamespaceListingFinished Hash (Result Http.Error NamespaceListing)
    | FetchRootNamespaceListingFinished (Result Http.Error NamespaceListing)
    | OpenDefinition Hash
    | FetchOpenDefinitionsFinished (List Hash) (Result Http.Error (List Definition))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        OpenDefinition hash ->
            ( { model
                | openDefinitions =
                    OpenDefinitions.insert hash
                        Loading
                        model.openDefinitions
              }
            , fetchOpenDefinitions [ hash ]
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


fetchOpenDefinitions : List Hash -> Cmd Msg
fetchOpenDefinitions hashes =
    Http.get
        { url = Api.definitionUrl (List.map Hash.toString hashes)
        , expect = Http.expectJson (FetchOpenDefinitionsFinished hashes) Definition.decodeList
        }



-- VIEW


viewDefinitionListing : DefinitionListing -> Html Msg
viewDefinitionListing definition =
    case definition of
        Type hash fqn ->
            a [ class "node type", onClick (OpenDefinition hash) ]
                [ UI.Icon.type_
                , label [] [ text (unqualifiedName fqn) ]
                ]

        Term hash fqn ->
            a [ class "node term", onClick (OpenDefinition hash) ]
                [ UI.Icon.term
                , label [] [ text (unqualifiedName fqn) ]
                ]

        Patch _ ->
            a [ class "node patch" ]
                [ UI.Icon.patch
                , label [] [ text "Patch" ]
                ]


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
            UI.errorMessage (Api.errorToString err)

        NotAsked ->
            UI.nothing

        Loading ->
            UI.spinner


viewNamespaceListing : HashSet -> NamespaceListing -> Html Msg
viewNamespaceListing expandedNamespaceListings (NamespaceListing hash fqn content) =
    let
        ( caretIcon, namespaceContent ) =
            if HashSet.member hash expandedNamespaceListings then
                ( UI.Icon.caretDown
                , div [ class "namespace-content" ]
                    [ viewNamespaceListingContent
                        expandedNamespaceListings
                        content
                    ]
                )

            else
                ( UI.Icon.caretRight, UI.nothing )
    in
    div []
        [ a
            [ class "node namespace"
            , onClick (ToggleExpandedNamespaceListing hash)
            ]
            [ caretIcon, label [] [ text (unqualifiedName fqn) ] ]
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
                    UI.errorMessage (Api.errorToString err)

                NotAsked ->
                    UI.spinner

                Loading ->
                    UI.spinner
    in
    div [ id "all-namespaces", class "namespace-tree" ]
        [ h2 [] [ text "All Namespaces" ]
        , listings
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


viewDefinition : WebData Definition -> Html Msg
viewDefinition definition =
    let
        row content =
            div [ class "definition-row" ] [ content ]
    in
    case definition of
        Success def ->
            row (text (Definition.unqualifiedName def))

        Failure err ->
            row (UI.errorMessage (Api.errorToString err))

        NotAsked ->
            UI.nothing

        Loading ->
            row UI.spinner


viewOpenDefinitions : OpenDefinitions -> List (Html Msg)
viewOpenDefinitions openDefinitions =
    openDefinitions
        |> OpenDefinitions.definitions
        |> List.map viewDefinition


viewWorkspace : Model -> Html Msg
viewWorkspace model =
    article [ id "workspace" ]
        [ header [ id "workspace-toolbar" ] []
        , section [ id "workspace-pans" ]
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
