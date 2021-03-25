module App exposing (..)

import Api
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Definition exposing (Definition(..))
import Finder
import FullyQualifiedName as FQN exposing (FQN, unqualifiedName)
import FullyQualifiedNameSet as FQNSet exposing (FQNSet)
import HashQualified exposing (HashQualified)
import Html
    exposing
        ( Html
        , a
        , aside
        , div
        , h1
        , h2
        , header
        , label
        , span
        , text
        )
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key exposing (Key(..))
import NamespaceListing
    exposing
        ( DefinitionListing(..)
        , NamespaceListing(..)
        , NamespaceListingContent
        , TermCategory(..)
        , TypeCategory(..)
        )
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Icon as Icon
import Url exposing (Url)
import Workspace



-- MODEL


type Modal
    = NoModal
    | FinderModal Finder.Model


type alias Model =
    { navKey : Nav.Key
    , currentUrl : Url
    , workspace : Workspace.Model
    , rootNamespaceListing : WebData NamespaceListing
    , expandedNamespaceListings : FQNSet
    , modal : Modal
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ initialUrl navKey =
    let
        ( workspace, workspaceCmd ) =
            Workspace.init

        model =
            { navKey = navKey
            , currentUrl = initialUrl
            , workspace = workspace
            , rootNamespaceListing = Loading
            , expandedNamespaceListings = FQNSet.empty
            , modal = NoModal
            }
    in
    ( model
    , Cmd.batch [ fetchRootNamespaceListing, Cmd.map WorkspaceMsg workspaceCmd ]
    )



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | HandleKeyboardEvent KeyboardEvent
    | ToggleExpandedNamespaceListing FQN
    | FetchSubNamespaceListingFinished FQN (Result Http.Error NamespaceListing)
    | FetchRootNamespaceListingFinished (Result Http.Error NamespaceListing)
    | OpenDefinition HashQualified
      -- sub msgs
    | FinderMsg Finder.Msg
    | WorkspaceMsg Workspace.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        HandleKeyboardEvent event ->
            handleKeyboardEvent model event

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

        OpenDefinition hq ->
            let
                ( workspace, cmd ) =
                    Workspace.open model.workspace hq
            in
            ( { model | workspace = workspace }, Cmd.map WorkspaceMsg cmd )

        WorkspaceMsg wMsg ->
            let
                ( workspace, wCmd, out ) =
                    Workspace.update wMsg model.workspace

                ( newModel, fCmd ) =
                    case out of
                        Workspace.None ->
                            ( model, Cmd.none )

                        Workspace.ShowFinderRequest ->
                            showFinder model
            in
            ( { newModel | workspace = workspace }, Cmd.batch [ fCmd, Cmd.map WorkspaceMsg wCmd ] )

        FinderMsg fMsg ->
            case model.modal of
                NoModal ->
                    ( model, Cmd.none )

                FinderModal fModel ->
                    let
                        ( fm, fc, out ) =
                            Finder.update fMsg fModel
                    in
                    case out of
                        Finder.Remain ->
                            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fc )

                        Finder.Exit ->
                            ( { model | modal = NoModal }, Cmd.none )

                        Finder.OpenDefinition hash ->
                            let
                                ( workspace, cmd ) =
                                    Workspace.open model.workspace (HashQualified.HashOnly hash)
                            in
                            ( { model | workspace = workspace }, Cmd.map WorkspaceMsg cmd )



-- UPDATE HELPERS


handleKeyboardEvent : Model -> KeyboardEvent -> ( Model, Cmd Msg )
handleKeyboardEvent model keyboardEvent =
    case keyboardEvent.keyCode of
        K ->
            if keyboardEvent.ctrlKey || keyboardEvent.metaKey then
                showFinder model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


showFinder : { m | modal : Modal } -> ( { m | modal : Modal }, Cmd Msg )
showFinder model =
    let
        ( fm, fcmd ) =
            Finder.init
    in
    ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fcmd )



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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map HandleKeyboardEvent decodeKeyboardEvent)
        , Sub.map WorkspaceMsg (Workspace.subscriptions model.workspace)
        ]



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
            viewListingRow (Just (OpenDefinition (HashQualified.HashQualified fqn hash))) (unqualifiedName fqn)
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
                    viewNamespaceListingContent
                        expandedNamespaceListings
                        content

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


viewModal : Modal -> Html Msg
viewModal modal =
    case modal of
        NoModal ->
            UI.nothing

        FinderModal m ->
            Html.map FinderMsg (Finder.view m)


view : Model -> Browser.Document Msg
view model =
    { title = "Unison Codebase"
    , body =
        [ div [ id "app" ]
            [ viewMainSidebar model
            , Html.map WorkspaceMsg (Workspace.view model.workspace)
            , viewModal model.modal
            ]
        ]
    }
