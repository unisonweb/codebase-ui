module App exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import CodebaseTree
import Definition.Reference exposing (Reference)
import Env exposing (Env, OperatingSystem(..))
import Env.AppContext as AppContext exposing (AppContext(..))
import Finder
import Finder.SearchOptions as SearchOptions
import FullyQualifiedName as FQN exposing (FQN)
import Html exposing (Html, a, div, h1, h2, h3, nav, p, section, span, strong, text)
import Html.Attributes exposing (class, classList, href, id, rel, target, title)
import Html.Events exposing (onClick)
import Http
import KeyboardShortcut
import KeyboardShortcut.Key as Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import Namespace exposing (NamespaceDetails)
import Perspective exposing (Perspective(..))
import PerspectiveLanding
import RemoteData
import Route exposing (Route)
import UI
import UI.AppHeader as AppHeader
import UI.Banner as Banner
import UI.Button as Button
import UI.Click as Click exposing (Click(..))
import UI.CopyField as CopyField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.Sidebar as Sidebar
import UI.Tooltip as Tooltip
import UnisonShare.SidebarContent
import Url exposing (Url)
import Workspace
import Workspace.WorkspaceItems as WorkspaceItems



-- MODEL


type Modal
    = NoModal
    | FinderModal Finder.Model
    | HelpModal
    | ReportBugModal
    | PublishModal
    | DownloadModal FQN


type alias Model =
    { navKey : Nav.Key
    , route : Route
    , codebaseTree : CodebaseTree.Model
    , workspace : Workspace.Model
    , perspectiveLanding : PerspectiveLanding.Model
    , modal : Modal
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

        model =
            { navKey = navKey
            , route = route
            , workspace = workspace
            , perspectiveLanding = PerspectiveLanding.init
            , codebaseTree = codebaseTree
            , modal = NoModal
            , keyboardShortcut = KeyboardShortcut.init env.operatingSystem
            , env = env
            , sidebarToggled = False
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , Cmd.map WorkspaceMsg workspaceCmd
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
    | ShowModal Modal
    | CloseModal
    | ToggleSidebar
      -- sub msgs
    | FinderMsg Finder.Msg
    | WorkspaceMsg Workspace.Msg
    | PerspectiveLandingMsg PerspectiveLanding.Msg
    | CodebaseTreeMsg CodebaseTree.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ env } as model) =
    case msg of
        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            -- URL changes happen when setting focus on a definitions.
            -- Currently, the URL change is a result of that as oppose to focus
            -- being a result of a URL change
            ( { model | route = Route.fromUrl env.basePath url }, Cmd.none )

        ChangePerspective perspective ->
            replacePerspective model perspective

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
            openDefinition model ref

        ShowModal modal ->
            ( { model | modal = modal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        ToggleSidebar ->
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )

        -- Sub msgs
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
                    openDefinition model2 ref

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
                            openDefinition model4 ref

                        CodebaseTree.ChangePerspectiveToNamespace fqn ->
                            fqn
                                |> Perspective.toNamespacePerspective model.env.perspective
                                |> replacePerspective model
            in
            ( model3, Cmd.batch [ cmd, Cmd.map CodebaseTreeMsg cCmd ] )

        FinderMsg fMsg ->
            case model.modal of
                FinderModal fModel ->
                    let
                        ( fm, fc, out ) =
                            Finder.update env fMsg fModel
                    in
                    case out of
                        Finder.Remain ->
                            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fc )

                        Finder.Exit ->
                            ( { model | modal = NoModal }, Cmd.none )

                        Finder.OpenDefinition ref ->
                            openDefinition { model | modal = NoModal } ref

                _ ->
                    ( model, Cmd.none )

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
            Workspace.open model.env model.workspace ref

        model2 =
            { model | workspace = workspace }

        ( model3, cmd ) =
            handleWorkspaceOutMsg model2 outMsg
    in
    ( model3, Cmd.batch [ cmd, Cmd.map WorkspaceMsg wCmd ] )


replacePerspective : Model -> Perspective -> ( Model, Cmd Msg )
replacePerspective ({ env } as model) perspective =
    let
        newEnv =
            { env | perspective = perspective }

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init newEnv

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

        fetchNamespaceDetailsCmd =
            perspective
                |> fetchNamespaceDetails
                |> Maybe.map (Api.perform env.apiBasePath)
                |> Maybe.withDefault Cmd.none
    in
    ( { model | env = newEnv, codebaseTree = codebaseTree, workspace = workspace }
    , Cmd.batch
        [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , changeRouteCmd
        , fetchNamespaceDetailsCmd
        ]
    )


handleWorkspaceOutMsg : Model -> Workspace.OutMsg -> ( Model, Cmd Msg )
handleWorkspaceOutMsg model out =
    case out of
        Workspace.None ->
            ( model, Cmd.none )

        Workspace.ShowFinderRequest withinNamespace ->
            showFinder model withinNamespace

        Workspace.Focused ref ->
            ( model, Route.navigateToByReference model.navKey model.route ref )

        Workspace.Emptied ->
            ( model, Route.navigateToCurrentPerspective model.navKey model.route )

        Workspace.ChangePerspectiveToNamespace fqn ->
            fqn
                |> Perspective.toNamespacePerspective model.env.perspective
                |> replacePerspective model


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
            ( { model | modal = HelpModal }, Cmd.none )

        KeyboardShortcut.Sequence _ Escape ->
            if model.modal == HelpModal then
                ( { model | modal = NoModal }, Cmd.none )

            else
                noOp

        _ ->
            noOp


showFinder :
    { m | env : Env, modal : Modal }
    -> Maybe FQN
    -> ( { m | env : Env, modal : Modal }, Cmd Msg )
showFinder model withinNamespace =
    let
        options =
            SearchOptions.init model.env.perspective withinNamespace

        ( fm, fcmd ) =
            Finder.init model.env options
    in
    ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fcmd )



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


appTitle : Maybe msg -> AppContext -> AppHeader.AppTitle msg
appTitle clickMsg appContext =
    let
        appTitle_ =
            case clickMsg of
                Nothing ->
                    AppHeader.Disabled

                Just msg ->
                    AppHeader.Clickable msg

        content =
            case appContext of
                UnisonLocal ->
                    h1 [] [ text "Unison", span [ class "context unison-local" ] [ text "Local" ] ]

                UnisonShare ->
                    h1 [] [ text "Unison", span [ class "context unison-share" ] [ text "Share" ] ]
    in
    appTitle_ content


viewAppHeader : Model -> Html Msg
viewAppHeader model =
    let
        { appContext, perspective } =
            model.env

        changePerspectiveMsg =
            case perspective of
                Codebase codebaseHash ->
                    ChangePerspective (Codebase codebaseHash)

                Namespace { codebaseHash } ->
                    ChangePerspective (Codebase codebaseHash)

        appTitle_ =
            appTitle (Just changePerspectiveMsg) appContext

        banner =
            case appContext of
                UnisonLocal ->
                    Nothing

                UnisonShare ->
                    Just
                        (Banner.promotion "article"
                            "New Article: Spark-like distributed datasets in under 100 lines of Unison"
                            (ExternalHref "https://www.unison-lang.org/articles/distributed-datasets/")
                            "Check it out!"
                        )
    in
    AppHeader.view
        { menuToggle = Just ToggleSidebar
        , appTitle = appTitle_
        , banner = banner
        , rightButton = Just (Button.button (ShowModal PublishModal) "Publish on Unison Share" |> Button.share)
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
                    case env.appContext of
                        UnisonShare ->
                            Button.iconThenLabel (ShowModal (DownloadModal fqn)) Icon.download "Download latest version"
                                |> Button.small
                                |> Button.view
                                |> List.singleton
                                |> Sidebar.headerItem []

                        Ucm ->
                            UI.nothing
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


subMenu : AppContext -> List ( String, Click Msg )
subMenu appContext =
    [ ( "Unison website", ExternalHref "https://unisonweb.org" )
    , ( "Docs", ExternalHref "https://unisonweb.org/docs" )
    , ( "Language Reference", ExternalHref "https://unisonweb.org/docs/language-reference" )
    , ( "Community", ExternalHref "https://unisonweb.org/community" )
    , ( "Report a bug", OnClick (ShowModal ReportBugModal) )
    ]
        ++ (if AppContext.isUnisonLocal appContext then
                [ ( "Unison Share", ExternalHref "https://share.unison-lang.org" ) ]

            else
                []
           )


unisonSubmenu : AppContext -> Html Msg
unisonSubmenu appContext =
    Tooltip.tooltip
        (Icon.unisonMark
            |> Icon.withClass "sidebar-unison-submenu"
            |> Icon.view
        )
        (subMenu appContext |> Tooltip.textMenu)
        |> Tooltip.withPosition Tooltip.RightOf
        |> Tooltip.withArrow Tooltip.End
        |> Tooltip.view


viewMainSidebar : Model -> Html Msg
viewMainSidebar model =
    let
        perspective =
            model.env.perspective

        appContext =
            model.env.appContext

        changePerspectiveMsg =
            Perspective.toNamespacePerspective perspective >> ChangePerspective

        sidebarContent =
            if Perspective.isCodebasePerspective perspective && AppContext.isUnisonShare appContext then
                UnisonShare.SidebarContent.view changePerspectiveMsg

            else
                UI.nothing
    in
    Sidebar.view
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
                        (subMenu appContext)
                        ++ [ a [ class "show-help", onClick (ShowModal HelpModal) ]
                                [ text "Keyboard Shortcuts"
                                , KeyboardShortcut.view model.keyboardShortcut (KeyboardShortcut.single QuestionMark)
                                ]
                           ]
                    )
                ]
            ]
        , div [ class "collapsed-content" ]
            [ unisonSubmenu appContext
            , Tooltip.tooltip
                (a
                    [ class "show-help-collapsed", onClick (ShowModal HelpModal) ]
                    [ KeyboardShortcut.view model.keyboardShortcut (KeyboardShortcut.single QuestionMark)
                    ]
                )
                (Tooltip.Text "Keyboard Shortcuts")
                |> Tooltip.withPosition Tooltip.RightOf
                |> Tooltip.withArrow Tooltip.Middle
                |> Tooltip.view
            ]
        ]


viewDownloadModal : FQN -> Html Msg
viewDownloadModal fqn =
    let
        prettyName =
            FQN.toString fqn

        unqualified =
            FQN.unqualifiedName fqn

        pullCommand =
            "pull git@github.com:unisonweb/share.git:." ++ prettyName ++ " ." ++ unqualified

        content =
            Modal.Content
                (section
                    []
                    [ p [] [ text "Download ", UI.bold prettyName, text " by pulling the namespace from Unison Share into a namespace in your local codebase:" ]
                    , CopyField.copyField (\_ -> CloseModal) pullCommand |> CopyField.withPrefix ".>" |> CopyField.view
                    , div [ class "hint" ] [ text "Copy and paste this command into UCM." ]
                    ]
                )
    in
    Modal.modal "download-modal" CloseModal content
        |> Modal.withHeader ("Download " ++ prettyName)
        |> Modal.view


viewHelpModal : OperatingSystem -> KeyboardShortcut.Model -> Html Msg
viewHelpModal os keyboardShortcut =
    let
        viewRow label instructions =
            div
                [ class "row" ]
                [ label
                , div [ class "instructions" ] instructions
                ]

        viewInstructions label shortcuts =
            viewRow label [ KeyboardShortcut.viewShortcuts keyboardShortcut shortcuts ]

        openFinderInstructions =
            case os of
                MacOS ->
                    [ KeyboardShortcut.Chord Meta (K Key.Lower), KeyboardShortcut.Chord Ctrl (K Key.Lower), KeyboardShortcut.single ForwardSlash ]

                _ ->
                    [ KeyboardShortcut.Chord Ctrl (K Key.Lower), KeyboardShortcut.single ForwardSlash ]

        toggleSidebarInstructions =
            case os of
                MacOS ->
                    [ KeyboardShortcut.Chord Meta (B Key.Lower), KeyboardShortcut.Chord Ctrl (B Key.Lower) ]

                _ ->
                    [ KeyboardShortcut.Chord Ctrl (B Key.Lower), KeyboardShortcut.single (S Key.Lower) ]

        content =
            Modal.Content
                (section
                    [ class "shortcuts" ]
                    [ div [ class "shortcut-group" ]
                        [ h3 [] [ text "General" ]
                        , viewInstructions (span [] [ text "Keyboard shortcuts", UI.subtle " (this dialog)" ]) [ KeyboardShortcut.single QuestionMark ]
                        , viewInstructions (text "Open Finder") openFinderInstructions
                        , viewInstructions (text "Toggle sidebar") toggleSidebarInstructions
                        , viewInstructions (text "Move focus up") [ KeyboardShortcut.single ArrowUp, KeyboardShortcut.single (K Key.Lower) ]
                        , viewInstructions (text "Move focus down") [ KeyboardShortcut.single ArrowDown, KeyboardShortcut.single (J Key.Lower) ]
                        , viewInstructions (text "Close focused definition") [ KeyboardShortcut.single (X Key.Lower) ]
                        , viewInstructions (text "Expand/Collapse focused definition") [ KeyboardShortcut.single Space ]
                        ]
                    , div [ class "shortcut-group" ]
                        [ h3 [] [ text "Finder" ]
                        , viewInstructions (text "Clear search query") [ KeyboardShortcut.single Escape ]
                        , viewInstructions (span [] [ text "Close", UI.subtle " (when search query is empty)" ]) [ KeyboardShortcut.single Escape ]
                        , viewInstructions (text "Move focus up") [ KeyboardShortcut.single ArrowUp ]
                        , viewInstructions (text "Move focus down") [ KeyboardShortcut.single ArrowDown ]
                        , viewInstructions (text "Open focused definition") [ KeyboardShortcut.single Enter ]
                        , viewRow (text "Open definition")
                            [ KeyboardShortcut.viewBase
                                [ KeyboardShortcut.viewKey os Semicolon False
                                , KeyboardShortcut.viewThen
                                , KeyboardShortcut.viewKeyBase "1-9" False
                                ]
                            ]
                        ]
                    ]
                )
    in
    Modal.modal "help-modal" CloseModal content
        |> Modal.withHeader "Keyboard shortcuts"
        |> Modal.view


githubLinkButton : String -> Html msg
githubLinkButton repo =
    Button.linkIconThenLabel ("https://github.com/" ++ repo) Icon.github repo
        |> Button.small
        |> Button.contained
        |> Button.view


viewPublishModal : Html Msg
viewPublishModal =
    let
        content =
            Modal.Content
                (section
                    []
                    [ p [ class "main" ]
                        [ text "With your Unison codebase on GitHub, open a Pull Request against "
                        , githubLinkButton "unisonweb/share"
                        , text " to list (or unlist) your project on Unison Share."
                        ]
                    , a [ class "help", href "https://www.unisonweb.org/docs/codebase-organization/#day-to-day-development-creating-and-merging-pull-requests", rel "noopener", target "_blank" ] [ text "How do I get my code on GitHub?" ]
                    ]
                )
    in
    Modal.modal "publish-modal" CloseModal content
        |> Modal.withHeader "Publish your project on Unison Share"
        |> Modal.view


viewReportBugModal : AppContext -> Html Msg
viewReportBugModal appContext =
    let
        content =
            Modal.Content
                (div []
                    [ section []
                        [ p [] [ text "We try our best, but bugs unfortunately creep through :(" ]
                        , p [] [ text "We greatly appreciate feedback and bug reports—its very helpful for providing the best developer experience when working with Unison." ]
                        ]
                    , UI.divider
                    , section [ class "actions" ]
                        [ p [] [ text "Visit our GitHub repositories to report bugs and provide feedback" ]
                        , div [ class "action" ]
                            [ githubLinkButton "unisonweb/codebase-ui"
                            , text "for reports on"
                            , strong [] [ text (AppContext.toString appContext) ]
                            , span [ class "subtle" ] [ text "(this UI)" ]
                            ]
                        , div [ class "action" ]
                            [ githubLinkButton "unisonweb/unison"
                            , text "for reports on the"
                            , strong [] [ text "Unison Language" ]
                            , span [ class "subtle" ] [ text "(UCM)" ]
                            ]
                        ]
                    ]
                )
    in
    Modal.modal "report-bug-modal" CloseModal content
        |> Modal.withHeader "Report a Bug"
        |> Modal.view


viewModal :
    { m | env : Env, modal : Modal, keyboardShortcut : KeyboardShortcut.Model }
    -> Html Msg
viewModal model =
    case model.modal of
        NoModal ->
            UI.nothing

        FinderModal m ->
            Html.map FinderMsg (Finder.view m)

        HelpModal ->
            viewHelpModal model.env.operatingSystem model.keyboardShortcut

        PublishModal ->
            viewPublishModal

        ReportBugModal ->
            viewReportBugModal model.env.appContext

        DownloadModal fqn ->
            viewDownloadModal fqn


viewAppLoading : AppContext -> Html msg
viewAppLoading appContext =
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Nothing appContext))
        , Sidebar.view []
        , div [ id "main-content" ] []
        ]


viewAppError : AppContext -> Http.Error -> Html msg
viewAppError appContext error =
    let
        context =
            AppContext.toString appContext
    in
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Nothing appContext))
        , Sidebar.view []
        , div [ id "main-content", class "app-error" ]
            [ Icon.view Icon.warn
            , p [ title (Api.errorToString error) ]
                [ text (context ++ " could not be started.") ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        title_ =
            AppContext.toString model.env.appContext

        page =
            case model.route of
                Route.Perspective _ ->
                    Html.map PerspectiveLandingMsg
                        (PerspectiveLanding.view
                            model.env
                            model.perspectiveLanding
                        )

                Route.Definition _ _ ->
                    Html.map WorkspaceMsg (Workspace.view model.workspace)
    in
    { title = title_
    , body =
        [ div [ id "app", classList [ ( "sidebar-toggled", model.sidebarToggled ) ] ]
            [ viewAppHeader model
            , viewMainSidebar model
            , div [ id "main-content" ] [ page ]
            , viewModal model
            ]
        ]
    }
