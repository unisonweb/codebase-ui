module App exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Definition.Category as Category
import Finder
import FullyQualifiedName as FQN exposing (FQN, unqualifiedName)
import FullyQualifiedNameSet as FQNSet exposing (FQNSet)
import HashQualified exposing (HashQualified(..))
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
import KeyboardShortcut
import KeyboardShortcut.Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import NamespaceListing
    exposing
        ( DefinitionListing(..)
        , NamespaceListing(..)
        , NamespaceListingContent
        )
import RelativeTo exposing (RelativeTo(..))
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import UI
import UI.Icon as Icon
import Url exposing (Url)
import Workspace
import Workspace.Reference exposing (Reference(..))



-- MODEL


type Modal
    = NoModal
    | FinderModal Finder.Model


type alias Model =
    { navKey : Nav.Key
    , route : Route
    , relativeTo : RelativeTo
    , workspace : Workspace.Model
    , rootNamespaceListing : WebData NamespaceListing
    , expandedNamespaceListings : FQNSet
    , modal : Modal
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        route =
            Route.fromUrl url

        ( workspace, workspaceCmd ) =
            case route of
                Route.ByReference _ ref ->
                    Workspace.init (Just ref)

                _ ->
                    Workspace.init Nothing

        {--| TODO: When we can't get a relative to, we need to resolve the name
          in the url to a hash-}
        relativeTo =
            Maybe.withDefault Codebase (Route.relativeTo route)

        model =
            { navKey = navKey
            , route = route
            , relativeTo = relativeTo
            , workspace = workspace
            , rootNamespaceListing = Loading
            , expandedNamespaceListings = FQNSet.empty
            , modal = NoModal
            , keyboardShortcut = KeyboardShortcut.init
            }
    in
    ( model
    , Cmd.batch [ fetchRootNamespaceListing, Cmd.map WorkspaceMsg workspaceCmd ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Keydown KeyboardEvent
    | ToggleExpandedNamespaceListing FQN
    | FetchSubNamespaceListingFinished FQN (Result Http.Error NamespaceListing)
    | FetchRootNamespaceListingFinished (Result Http.Error NamespaceListing)
    | OpenDefinition Reference
      -- sub msgs
    | FinderMsg Finder.Msg
    | WorkspaceMsg Workspace.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            -- URL changes happen when setting focus on a definitions.
            -- Currently, the URL change is a result of that as oppose to focus
            -- being a result of a URL change
            ( { model | route = Route.fromUrl url }, Cmd.none )

        Keydown event ->
            keydown model event

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

                namespaceContentFetched =
                    model.rootNamespaceListing
                        |> RemoteData.map (\nl -> NamespaceListing.contentFetched nl fqn)
                        |> RemoteData.withDefault False

                cmd =
                    if shouldExpand && not namespaceContentFetched then
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

        OpenDefinition ref ->
            openDefinition model ref

        WorkspaceMsg wMsg ->
            let
                ( workspace, wCmd, outMsg ) =
                    Workspace.update wMsg model.workspace

                model2 =
                    { model | workspace = workspace }

                ( model3, cmd ) =
                    handleWorkspaceOutMsg model2 outMsg
            in
            ( model3, Cmd.batch [ cmd, Cmd.map WorkspaceMsg wCmd ] )

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

                        Finder.OpenDefinition ref ->
                            openDefinition { model | modal = NoModal } ref

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )



-- UPDATE HELPERS


openDefinition : Model -> Reference -> ( Model, Cmd Msg )
openDefinition model ref =
    let
        ( workspace, wCmd, outMsg ) =
            Workspace.open model.workspace ref

        model2 =
            { model | workspace = workspace }

        ( model3, cmd ) =
            handleWorkspaceOutMsg model2 outMsg
    in
    ( model3, Cmd.batch [ cmd, Cmd.map WorkspaceMsg wCmd ] )


handleWorkspaceOutMsg : Model -> Workspace.OutMsg -> ( Model, Cmd Msg )
handleWorkspaceOutMsg model out =
    case out of
        Workspace.None ->
            ( model, Cmd.none )

        Workspace.ShowFinderRequest ->
            showFinder model

        Workspace.Focused ref ->
            ( model, Route.navigateToByReference model.navKey model.route ref )

        Workspace.Emptied ->
            ( model, Route.navigateToLatest model.navKey )


keydown : Model -> KeyboardEvent -> ( Model, Cmd Msg )
keydown model keyboardEvent =
    let
        shortcut =
            KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut keyboardEvent
    in
    case shortcut of
        KeyboardShortcut.Chord Ctrl (K _) ->
            showFinder model

        KeyboardShortcut.Chord Meta (K _) ->
            showFinder model

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
        [ KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
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
        viewDefRow ref fqn =
            viewListingRow (Just (OpenDefinition ref)) (unqualifiedName fqn)
    in
    case listing of
        TypeListing hash fqn category ->
            viewDefRow (TypeReference (HashOnly hash)) fqn (Category.name category) (Category.icon category)

        TermListing hash fqn category ->
            viewDefRow (TermReference (HashOnly hash)) fqn (Category.name category) (Category.icon category)

        DataConstructorListing hash fqn ->
            viewDefRow (DataConstructorReference (HashOnly hash)) fqn "constructor" Icon.DataConstructor

        AbilityConstructorListing hash fqn ->
            viewDefRow (AbilityConstructorReference (HashOnly hash)) fqn "constructor" Icon.AbilityConstructor

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
