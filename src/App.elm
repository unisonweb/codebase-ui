module App exposing (..)

import Api
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Definition exposing (Definition(..))
import FullyQualifiedName as FQN exposing (FQN, unqualifiedName)
import FullyQualifiedNameSet as FQNSet exposing (FQNSet)
import Hash exposing (Hash)
import Html
    exposing
        ( Html
        , a
        , article
        , aside
        , div
        , h1
        , h2
        , header
        , label
        , section
        , span
        , text
        )
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import NamespaceListing
    exposing
        ( DefinitionListing(..)
        , NamespaceListing(..)
        , NamespaceListingContent
        , TermCategory(..)
        , TypeCategory(..)
        )
import OpenDefinitions exposing (HashIndexedDefinition, OpenDefinitions)
import RemoteData exposing (RemoteData(..), WebData)
import Task
import UI
import UI.Icon as Icon
import Url exposing (Url)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , currentUrl : Url
    , openDefinitions : OpenDefinitions
    , rootNamespaceListing : WebData NamespaceListing
    , expandedNamespaceListings : FQNSet
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ initialUrl navKey =
    let
        model =
            { navKey = navKey
            , currentUrl = initialUrl
            , openDefinitions = OpenDefinitions.init Nothing
            , rootNamespaceListing = Loading
            , expandedNamespaceListings = FQNSet.empty
            }
    in
    ( model, fetchRootNamespaceListing )



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ToggleExpandedNamespaceListing FQN
    | FetchSubNamespaceListingFinished FQN (Result Http.Error NamespaceListing)
    | FetchRootNamespaceListingFinished (Result Http.Error NamespaceListing)
    | OpenDefinition Hash
    | OpenDefinitionAfter Hash Hash
    | CloseDefinition Hash
    | FetchOpenDefinitionFinished Hash (WebData Definition)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

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
            openDefinition model Nothing hash

        OpenDefinitionAfter afterHash hash ->
            openDefinition model (Just afterHash) hash

        CloseDefinition hash ->
            ( { model
                | openDefinitions =
                    OpenDefinitions.remove hash
                        model.openDefinitions
              }
            , Cmd.none
            )

        FetchOpenDefinitionFinished hash response ->
            let
                nextOpenDefinitions =
                    OpenDefinitions.replace hash response model.openDefinitions
            in
            ( { model | openDefinitions = nextOpenDefinitions }, Cmd.none )



-- UPDATE HELPERS


openDefinition :
    { m | openDefinitions : OpenDefinitions }
    -> Maybe Hash
    -> Hash
    -> ( { m | openDefinitions : OpenDefinitions }, Cmd Msg )
openDefinition model afterHash hash =
    -- We don't want to refetch or replace any already open definitions, but we
    -- do want to scroll to it
    if OpenDefinitions.member hash model.openDefinitions then
        ( model, scrollToDefinition hash )

    else
        let
            toInsert =
                HashIndexedDefinition hash Loading

            insert =
                case afterHash of
                    Nothing ->
                        OpenDefinitions.insertWithFocus toInsert

                    Just h ->
                        OpenDefinitions.insertWithFocusAfter h toInsert

            nextOpenDefinitions =
                insert model.openDefinitions
        in
        ( { model | openDefinitions = nextOpenDefinitions }
        , Cmd.batch [ fetchDefinition hash, scrollToDefinition hash ]
        )



-- EFFECTS


fetchRootNamespaceListing : Cmd Msg
fetchRootNamespaceListing =
    let
        rootFqn =
            FQN.fromString "."
    in
    Http.get
        { url = Api.list Nothing
        , expect = Http.expectJson FetchRootNamespaceListingFinished (NamespaceListing.decode rootFqn)
        }


fetchSubNamespaceListing : FQN -> Cmd Msg
fetchSubNamespaceListing fqn =
    Http.get
        { url = Api.list (Just (FQN.toString fqn))
        , expect = Http.expectJson (FetchSubNamespaceListingFinished fqn) (NamespaceListing.decode fqn)
        }


fetchDefinition : Hash -> Cmd Msg
fetchDefinition hash =
    Http.get
        { url = Api.definitions [ Hash.toString hash ]
        , expect =
            Http.expectJson
                (RemoteData.fromResult
                    >> FetchOpenDefinitionFinished hash
                )
                Definition.decodeHead
        }


scrollToDefinition : Hash -> Cmd Msg
scrollToDefinition hash =
    let
        id =
            "definition-" ++ Hash.toString hash
    in
    Task.sequence
        [ Dom.getElement id |> Task.map (.element >> .y)
        , Dom.getElement "workspace-content" |> Task.map (.element >> .y)
        , Dom.getViewportOf "workspace-content" |> Task.map (.viewport >> .y)
        ]
        |> Task.andThen
            (\outcome ->
                case outcome of
                    elY :: viewportY :: viewportScrollTop :: [] ->
                        Dom.setViewportOf "workspace-content" 0 (viewportScrollTop + (elY - viewportY))
                            |> Task.onError (\_ -> Task.succeed ())

                    _ ->
                        Task.succeed ()
            )
        |> Task.attempt (always NoOp)



-- VIEW


viewListingRow : Maybe msg -> String -> String -> Icon.Icon -> Html msg
viewListingRow clickMsg label_ category icon =
    let
        containerClass =
            class ("node " ++ category)

        container =
            clickMsg
                |> Maybe.map (\msg -> a [ containerClass, onClick msg ])
                |> Maybe.withDefault (span [ containerClass ])
    in
    container
        [ Icon.view icon
        , label [] [ text label_ ]
        , span [ class "definition-category" ] [ text category ]
        ]


viewDefinitionListing : DefinitionListing -> Html Msg
viewDefinitionListing listing =
    let
        viewDefRow hash fqn =
            viewListingRow (Just (OpenDefinition hash)) (unqualifiedName fqn)
    in
    case listing of
        TypeListing hash fqn category ->
            case category of
                DataType ->
                    viewDefRow hash fqn "type" Icon.Type

                AbilityType ->
                    viewDefRow hash fqn "ability" Icon.Ability

        TermListing hash fqn category ->
            case category of
                PlainTerm ->
                    viewDefRow hash fqn "term" Icon.Term

                TestTerm ->
                    viewDefRow hash fqn "test" Icon.Test

                DocTerm ->
                    viewDefRow hash fqn "doc" Icon.Doc

        PatchListing _ ->
            viewListingRow Nothing "Patch" "patch" Icon.Patch


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
viewNamespaceListing expandedNamespaceListings (NamespaceListing _ fqn content) =
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
                Success (NamespaceListing _ _ content) ->
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
            Definition.view (CloseDefinition hash) (OpenDefinitionAfter hash) def

        Failure err ->
            Definition.viewError (CloseDefinition hash) hash err

        NotAsked ->
            UI.nothing

        Loading ->
            Definition.viewLoading hash


viewOpenDefinitions : OpenDefinitions -> List (Html Msg)
viewOpenDefinitions =
    OpenDefinitions.mapToList (\hid _ -> viewDefinition hid.hash hid.definition)


viewWorkspace : Model -> Html Msg
viewWorkspace model =
    article [ id "workspace" ]
        [ header [ id "workspace-toolbar" ] [ UI.button "Open" NoOp ]
        , section [ id "workspace-content" ]
            [ section
                [ class "definitions-pane" ]
                (viewOpenDefinitions model.openDefinitions)
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Unison Codebase"
    , body =
        [ div [ id "app" ]
            [ viewMainSidebar model
            , viewWorkspace model
            ]
        ]
    }
