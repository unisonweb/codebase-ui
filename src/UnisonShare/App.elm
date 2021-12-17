module UnisonShare.App exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import CodebaseTree
import Definition.Reference exposing (Reference)
import Env exposing (Env)
import FullyQualifiedName as FQN exposing (FQN)
import Html exposing (Html, a, div, h1, h2, nav, p, span, text)
import Html.Attributes exposing (class, classList, id, title)
import Html.Events exposing (onClick)
import Http
import KeyboardShortcut
import KeyboardShortcut.Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import Namespace exposing (NamespaceDetails)
import Perspective exposing (Perspective(..))
import PerspectiveLanding
import RemoteData
import UI
import UI.AppHeader as AppHeader
import UI.Banner as Banner
import UI.Button as Button
import UI.Click as Click exposing (Click(..))
import UI.Icon as Icon
import UI.PageLayout as PageLayout
import UI.Sidebar as Sidebar
import UI.Tooltip as Tooltip
import UnisonShare.AppModal as AppModal
import UnisonShare.Page.Catalog as Catalog
import UnisonShare.Route as Route exposing (Route)
import Url exposing (Url)
import Workspace
import Workspace.WorkspaceItems as WorkspaceItems



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , route : Route
    , codebaseTree : CodebaseTree.Model
    , workspace : Workspace.Model
    , perspectiveLanding : PerspectiveLanding.Model
    , catalog : Catalog.Model
    , appModal : AppModal.Model
    , keyboardShortcut : KeyboardShortcut.Model
    , env : Env

    -- This is called "toggled" and not "hidden" because the behavior of
    -- toggling the sidebar on/off is inverse on mobile vs desktop
    , sidebarToggled : Bool
    }


init : Env -> Route -> Nav.Key -> ( Model, Cmd Msg )
init env route navKey =
    let
        ( workspace, workspaceCmd ) =
            case route of
                Route.Definition _ ref ->
                    Workspace.init env (Just ref)

                _ ->
                    Workspace.init env Nothing

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init env

        fetchNamespaceDetailsCmd =
            env.perspective
                |> fetchNamespaceDetails
                |> Maybe.map (Api.perform env.apiBasePath)
                |> Maybe.withDefault Cmd.none

        ( catalog, catalogCmd ) =
            Catalog.init env

        model =
            { navKey = navKey
            , route = route
            , workspace = workspace
            , perspectiveLanding = PerspectiveLanding.init
            , codebaseTree = codebaseTree
            , appModal = AppModal.init
            , keyboardShortcut = KeyboardShortcut.init env.operatingSystem
            , env = env
            , sidebarToggled = False
            , catalog = catalog
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , Cmd.map WorkspaceMsg workspaceCmd
        , Cmd.map CatalogMsg catalogCmd
        , fetchNamespaceDetailsCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ChangePerspective Perspective
    | FetchPerspectiveNamespaceDetailsFinished FQN (Result Http.Error NamespaceDetails)
    | Keydown KeyboardEvent
    | OpenDefinition Reference
    | ShowModal AppModal.AppModal
    | ToggleSidebar
      -- sub msgs
    | AppModalMsg AppModal.Msg
    | CatalogMsg Catalog.Msg
    | WorkspaceMsg Workspace.Msg
    | PerspectiveLandingMsg PerspectiveLanding.Msg
    | CodebaseTreeMsg CodebaseTree.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ env } as model) =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                -- External links are handled via target blank and never end up
                -- here
                Browser.External _ ->
                    ( model, Cmd.none )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl env.basePath url

                model2 =
                    { model | route = route }

                newEnv params =
                    { env | perspective = Perspective.nextFromParams env.perspective params }
            in
            case route of
                Route.Catalog ->
                    let
                        ( catalog, cmd ) =
                            Catalog.init model.env
                    in
                    ( { model2 | catalog = catalog }, Cmd.map CatalogMsg cmd )

                Route.Definition params ref ->
                    let
                        ( workspace, cmd ) =
                            Workspace.open (newEnv params) model.workspace ref

                        model3 =
                            { model2 | workspace = workspace, env = newEnv params }

                        ( model4, fetchPerspectiveCmd ) =
                            fetchPerspectiveAndCodebaseTree env.perspective model3
                    in
                    ( model4, Cmd.batch [ Cmd.map WorkspaceMsg cmd, fetchPerspectiveCmd ] )

                Route.Perspective params ->
                    fetchPerspectiveAndCodebaseTree env.perspective { model2 | env = newEnv params }

        ChangePerspective perspective ->
            navigateToPerspective model perspective

        FetchPerspectiveNamespaceDetailsFinished fqn details ->
            let
                perspective =
                    case env.perspective of
                        Namespace p ->
                            if FQN.equals p.fqn fqn then
                                Namespace { p | details = RemoteData.fromResult details }

                            else
                                env.perspective

                        _ ->
                            env.perspective

                nextEnv =
                    { env | perspective = perspective }
            in
            ( { model | env = nextEnv }, Cmd.none )

        Keydown event ->
            keydown model event

        OpenDefinition ref ->
            navigateToDefinition model ref

        ShowModal modal ->
            let
                ( appModal, cmd ) =
                    AppModal.show modal
            in
            ( { model | appModal = appModal }, Cmd.map AppModalMsg cmd )

        ToggleSidebar ->
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )

        -- Sub msgs
        AppModalMsg amMsg ->
            let
                ( am, amCmd, out ) =
                    AppModal.update env amMsg model.appModal

                ( newModel, cmd ) =
                    case out of
                        AppModal.OpenDefinition ref ->
                            navigateToDefinition { model | appModal = am } ref

                        _ ->
                            ( { model | appModal = am }, Cmd.none )
            in
            ( newModel, Cmd.batch [ Cmd.map AppModalMsg amCmd, cmd ] )

        CatalogMsg cMsg ->
            let
                ( catalog, cmd ) =
                    Catalog.update cMsg model.catalog
            in
            ( { model | catalog = catalog }, Cmd.map CatalogMsg cmd )

        WorkspaceMsg wMsg ->
            let
                ( workspace, wCmd, outMsg ) =
                    Workspace.update env wMsg model.workspace

                model2 =
                    { model | workspace = workspace }

                ( model3, cmd ) =
                    handleWorkspaceOutMsg model2 outMsg
            in
            ( model3, Cmd.batch [ cmd, Cmd.map WorkspaceMsg wCmd ] )

        PerspectiveLandingMsg rMsg ->
            let
                ( perspectiveLanding, outMsg ) =
                    PerspectiveLanding.update rMsg model.perspectiveLanding

                model2 =
                    { model | perspectiveLanding = perspectiveLanding }
            in
            case outMsg of
                PerspectiveLanding.OpenDefinition ref ->
                    navigateToDefinition model2 ref

                PerspectiveLanding.ShowFinderRequest ->
                    showFinder model2 Nothing

                PerspectiveLanding.None ->
                    ( model2, Cmd.none )

        CodebaseTreeMsg cMsg ->
            let
                ( codebaseTree, cCmd, outMsg ) =
                    CodebaseTree.update env cMsg model.codebaseTree

                model2 =
                    { model | codebaseTree = codebaseTree }

                ( model3, cmd ) =
                    case outMsg of
                        CodebaseTree.None ->
                            ( model2, Cmd.none )

                        CodebaseTree.OpenDefinition ref ->
                            -- reset sidebarToggled to close it on mobile, but keep it open on desktop
                            let
                                model4 =
                                    { model2 | sidebarToggled = False }
                            in
                            navigateToDefinition model4 ref

                        CodebaseTree.ChangePerspectiveToNamespace fqn ->
                            fqn
                                |> Perspective.toNamespacePerspective model.env.perspective
                                |> navigateToPerspective model
            in
            ( model3, Cmd.batch [ cmd, Cmd.map CodebaseTreeMsg cCmd ] )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )



-- UPDATE HELPERS


navigateToDefinition : Model -> Reference -> ( Model, Cmd Msg )
navigateToDefinition model ref =
    ( model, Route.navigateToDefinition model.navKey model.route ref )


navigateToPerspective : Model -> Perspective -> ( Model, Cmd Msg )
navigateToPerspective model perspective =
    let
        -- Update all open references to be hash based to ensure that we can
        -- refresh the page and fetch them appropriately even if they are
        -- outside of the current perspective
        workspace =
            Workspace.replaceWorkspaceItemReferencesWithHashOnly model.workspace

        -- Re-navigate to the currently open definition by hash
        focusedReferenceRoute =
            workspace.workspaceItems
                |> WorkspaceItems.focusedReference
                |> Maybe.map (Route.toDefinition model.route)
                |> Maybe.withDefault model.route

        changeRouteCmd =
            Route.replacePerspective model.navKey (Perspective.toParams perspective) focusedReferenceRoute
    in
    ( { model | workspace = workspace }, changeRouteCmd )


fetchPerspectiveAndCodebaseTree : Perspective -> Model -> ( Model, Cmd Msg )
fetchPerspectiveAndCodebaseTree oldPerspective ({ env } as model) =
    let
        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init env

        fetchNamespaceDetailsCmd =
            env.perspective
                |> fetchNamespaceDetails
                |> Maybe.map (Api.perform env.apiBasePath)
                |> Maybe.withDefault Cmd.none
    in
    if Perspective.needsFetching env.perspective then
        ( { model | codebaseTree = codebaseTree }
        , Cmd.batch
            [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
            , fetchNamespaceDetailsCmd
            ]
        )

    else if not (Perspective.equals oldPerspective env.perspective) then
        ( model, Cmd.map CodebaseTreeMsg codebaseTreeCmd )

    else
        ( model, Cmd.none )


handleWorkspaceOutMsg : Model -> Workspace.OutMsg -> ( Model, Cmd Msg )
handleWorkspaceOutMsg model out =
    case out of
        Workspace.None ->
            ( model, Cmd.none )

        Workspace.ShowFinderRequest withinNamespace ->
            showFinder model withinNamespace

        Workspace.Focused ref ->
            ( model, Route.navigateToDefinition model.navKey model.route ref )

        Workspace.Emptied ->
            ( model, Route.navigateToCurrentPerspective model.navKey model.route )

        Workspace.ChangePerspectiveToNamespace fqn ->
            fqn
                |> Perspective.toNamespacePerspective model.env.perspective
                |> navigateToPerspective model


keydown : Model -> KeyboardEvent -> ( Model, Cmd Msg )
keydown model keyboardEvent =
    let
        shortcut =
            KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut keyboardEvent

        noOp =
            ( model, Cmd.none )

        toggleSidebar =
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )
    in
    case shortcut of
        KeyboardShortcut.Chord Ctrl (K _) ->
            showFinder model Nothing

        KeyboardShortcut.Chord Meta (K _) ->
            if model.env.operatingSystem == Env.MacOS then
                showFinder model Nothing

            else
                noOp

        KeyboardShortcut.Chord Ctrl (B _) ->
            toggleSidebar

        KeyboardShortcut.Chord Meta (B _) ->
            toggleSidebar

        KeyboardShortcut.Sequence _ ForwardSlash ->
            showFinder model Nothing

        KeyboardShortcut.Chord Shift QuestionMark ->
            let
                ( am, amCmd ) =
                    AppModal.show AppModal.KeyboardShortcutsModal
            in
            ( { model | appModal = am }, Cmd.map AppModalMsg amCmd )

        -- TODO: Move exit by Escape into AppModal module
        KeyboardShortcut.Sequence _ Escape ->
            if AppModal.modalIs model.appModal AppModal.KeyboardShortcutsModal then
                ( { model | appModal = AppModal.close }, Cmd.none )

            else
                noOp

        _ ->
            noOp


showFinder : Model -> Maybe FQN -> ( Model, Cmd Msg )
showFinder model withinNamespace =
    let
        ( am, amCmd ) =
            AppModal.showFinder model.env withinNamespace
    in
    ( { model | appModal = am }, Cmd.map AppModalMsg amCmd )



-- EFFECTS


fetchNamespaceDetails : Perspective -> Maybe (Api.ApiRequest NamespaceDetails Msg)
fetchNamespaceDetails perspective =
    case perspective of
        Namespace { fqn } ->
            fqn
                |> Api.namespace perspective
                |> Api.toRequest Namespace.decodeDetails (FetchPerspectiveNamespaceDetailsFinished fqn)
                |> Just

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
        , Sub.map WorkspaceMsg (Workspace.subscriptions model.workspace)
        ]



-- VIEW


appTitle : Maybe msg -> AppHeader.AppTitle msg
appTitle clickMsg =
    let
        appTitle_ =
            case clickMsg of
                Nothing ->
                    AppHeader.Disabled

                Just msg ->
                    AppHeader.Clickable msg
    in
    appTitle_ (h1 [] [ text "Unison", span [ class "context unison-share" ] [ text "Share" ] ])


viewAppHeader : Model -> AppHeader.AppHeader Msg
viewAppHeader model =
    let
        changePerspectiveMsg =
            case model.env.perspective of
                Codebase codebaseHash ->
                    ChangePerspective (Codebase codebaseHash)

                Namespace { codebaseHash } ->
                    ChangePerspective (Codebase codebaseHash)

        appTitle_ =
            appTitle (Just changePerspectiveMsg)

        banner =
            Just
                (Banner.promotion "article"
                    "New Article: Spark-like distributed datasets in under 100 lines of Unison"
                    (ExternalHref "https://www.unison-lang.org/articles/distributed-datasets/")
                    "Check it out!"
                )
    in
    { menuToggle = Just ToggleSidebar
    , appTitle = appTitle_
    , banner = banner
    , rightButton = Just (Button.button (ShowModal AppModal.PublishModal) "Publish on Unison Share" |> Button.share)
    }


viewSidebarHeader : Env -> Html Msg
viewSidebarHeader env =
    case env.perspective of
        Codebase _ ->
            UI.nothing

        Namespace { fqn } ->
            let
                -- Imprecise, but close enough, approximation of overflowing,
                -- which results in a slight faded left edge A better way would
                -- be to measure the DOM like we do for overflowing docs, but
                -- thats quite involved...
                isOverflowing =
                    fqn |> FQN.toString |> String.length |> (\l -> l > 20)

                download =
                    Button.iconThenLabel (ShowModal (AppModal.DownloadModal fqn)) Icon.download "Download latest version"
                        |> Button.small
                        |> Button.view
                        |> List.singleton
                        |> Sidebar.headerItem []
            in
            Sidebar.header
                [ Sidebar.headerItem
                    [ classList [ ( "is-overflowing", isOverflowing ) ] ]
                    [ UI.namespaceSlug
                    , h2 [ class "namespace" ] [ FQN.view fqn ]
                    ]
                , download
                , UI.divider
                ]


viewMainSidebarCollapseButton : Model -> Html Msg
viewMainSidebarCollapseButton model =
    div
        [ class "collapse-sidebar-button" ]
        [ Button.icon ToggleSidebar
            (if model.sidebarToggled then
                Icon.chevronRight

             else
                Icon.chevronLeft
            )
            |> Button.small
            |> Button.view
        ]


subMenu : List ( String, Click Msg )
subMenu =
    [ ( "Unison website", ExternalHref "https://unisonweb.org" )
    , ( "Docs", ExternalHref "https://unisonweb.org/docs" )
    , ( "Language Reference", ExternalHref "https://unisonweb.org/docs/language-reference" )
    , ( "Community", ExternalHref "https://unisonweb.org/community" )
    , ( "Report a bug", OnClick (ShowModal AppModal.ReportBugModal) )
    ]


unisonSubmenu : Html Msg
unisonSubmenu =
    Tooltip.tooltip
        (Icon.unisonMark
            |> Icon.withClass "sidebar-unison-submenu"
            |> Icon.view
        )
        (Tooltip.textMenu subMenu)
        |> Tooltip.withPosition Tooltip.RightOf
        |> Tooltip.withArrow Tooltip.End
        |> Tooltip.view


viewMainSidebar : Model -> List (Html Msg)
viewMainSidebar model =
    let
        perspective =
            model.env.perspective

        changePerspectiveMsg =
            Perspective.toNamespacePerspective perspective >> ChangePerspective

        sidebarContent =
            if Perspective.isCodebasePerspective perspective then
                let
                    base =
                        FQN.fromString "unison.base"
                in
                Sidebar.section "Popular libraries"
                    [ Sidebar.item (changePerspectiveMsg base) (FQN.toString base)
                    ]

            else
                UI.nothing
    in
    [ viewMainSidebarCollapseButton model
    , div [ class "expanded-content" ]
        [ viewSidebarHeader model.env
        , div [ class "sidebar-scroll-area" ]
            [ sidebarContent
            , Sidebar.section
                "Namespaces and Definitions"
                [ Html.map CodebaseTreeMsg (CodebaseTree.view model.codebaseTree) ]
            , nav []
                (List.map
                    (\( l, c ) -> Click.view [] [ text l ] c)
                    subMenu
                    ++ [ a [ class "show-keyboard-shortcuts", onClick (ShowModal AppModal.KeyboardShortcutsModal) ]
                            [ text "Keyboard Shortcuts"
                            , KeyboardShortcut.view model.keyboardShortcut (KeyboardShortcut.single QuestionMark)
                            ]
                       ]
                )
            ]
        ]
    , div [ class "collapsed-content" ]
        [ unisonSubmenu
        , Tooltip.tooltip
            (a
                [ class "show-help-collapsed", onClick (ShowModal AppModal.KeyboardShortcutsModal) ]
                [ KeyboardShortcut.view model.keyboardShortcut (KeyboardShortcut.single QuestionMark)
                ]
            )
            (Tooltip.Text "Keyboard Shortcuts")
            |> Tooltip.withPosition Tooltip.RightOf
            |> Tooltip.withArrow Tooltip.Middle
            |> Tooltip.view
        ]
    ]


viewAppLoading : Html msg
viewAppLoading =
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Nothing))
        , PageLayout.view
            (PageLayout.FullLayout
                { content = PageLayout.PageContent [] }
            )
        ]


viewAppError : Http.Error -> Html msg
viewAppError error =
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Nothing))
        , PageLayout.view
            (PageLayout.FullLayout
                { content =
                    PageLayout.PageContent
                        [ div [ class "app-error" ]
                            [ Icon.view Icon.warn
                            , p [ title (Api.errorToString error) ]
                                [ text "Unison Share could not be started." ]
                            ]
                        ]
                }
            )
        ]


view : Model -> Browser.Document Msg
view model =
    let
        appHeader =
            AppHeader.view (viewAppHeader model)

        withSidebar pageContent =
            PageLayout.SidebarLayout
                { sidebar = viewMainSidebar model
                , sidebarToggled = model.sidebarToggled
                , content = PageLayout.PageContent [ pageContent ]
                }

        page =
            case model.route of
                Route.Catalog ->
                    Html.map CatalogMsg (Catalog.view model.catalog)

                Route.Perspective _ ->
                    Html.map PerspectiveLandingMsg
                        (PerspectiveLanding.view
                            model.env
                            model.perspectiveLanding
                        )
                        |> withSidebar
                        |> PageLayout.view

                Route.Definition _ _ ->
                    Html.map WorkspaceMsg (Workspace.view model.workspace)
                        |> withSidebar
                        |> PageLayout.view
    in
    { title = "Unison Share"
    , body =
        [ div [ id "app" ]
            [ appHeader
            , page
            , Html.map AppModalMsg (AppModal.view model.env model.appModal)
            ]
        ]
    }
