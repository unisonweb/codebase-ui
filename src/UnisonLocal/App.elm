module UnisonLocal.App exposing (..)

import Browser
import Browser.Navigation as Nav
import Code.CodebaseTree as CodebaseTree
import Code.Definition.Reference exposing (Reference)
import Code.Finder as Finder
import Code.Finder.SearchOptions as SearchOptions
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hashvatar as Hashvatar
import Code.Namespace as Namespace exposing (NamespaceDetails)
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.Workspace as Workspace
import Code.Workspace.WorkspaceItems as WorkspaceItems
import Html exposing (Html, a, div, h1, h2, h3, nav, p, section, span, strong, text)
import Html.Attributes exposing (class, classList, href, id, rel, target, title)
import Html.Events exposing (onClick)
import Http
import Lib.HttpApi as HttpApi
import Lib.OperatingSystem exposing (OperatingSystem(..))
import Lib.Util as Util
import RemoteData
import UI
import UI.AppHeader as AppHeader
import UI.Button as Button
import UI.Click as Click exposing (Click(..))
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.Modal as Modal
import UI.PageLayout as PageLayout
import UI.Sidebar as Sidebar
import UI.Tooltip as Tooltip
import UnisonLocal.Api as LocalApi
import UnisonLocal.Env as Env exposing (Env)
import UnisonLocal.PerspectiveLanding as PerspectiveLanding
import UnisonLocal.Route as Route exposing (Route)
import Url exposing (Url)



-- MODEL


type Modal
    = NoModal
    | FinderModal Finder.Model
    | HelpModal
    | ReportBugModal
    | PublishModal


type alias Model =
    { route : Route
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


init : Env -> Route -> ( Model, Cmd Msg )
init env route =
    let
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpointUrl env

        ( workspace, workspaceCmd ) =
            case route of
                Route.Definition _ ref ->
                    Workspace.init codebaseConfig (Just ref)

                _ ->
                    Workspace.init codebaseConfig Nothing

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init codebaseConfig

        fetchNamespaceDetailsCmd =
            env.perspective
                |> fetchNamespaceDetails
                |> Maybe.map (HttpApi.perform env.apiBasePath)
                |> Maybe.withDefault Cmd.none

        model =
            { route = route
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
    let
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpointUrl env
    in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl env.navKey (Url.toString url) )

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
                Route.Definition params ref ->
                    let
                        codebaseConfig_ =
                            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpointUrl (newEnv params)

                        ( workspace, cmd ) =
                            Workspace.open codebaseConfig_ model.workspace ref

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
            ( { model | modal = modal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        ToggleSidebar ->
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )

        -- Sub msgs
        WorkspaceMsg wMsg ->
            let
                ( workspace, wCmd, outMsg ) =
                    Workspace.update codebaseConfig wMsg model.workspace

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
                    CodebaseTree.update codebaseConfig cMsg model.codebaseTree

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

        FinderMsg fMsg ->
            case model.modal of
                FinderModal fModel ->
                    let
                        ( fm, fc, out ) =
                            Finder.update codebaseConfig fMsg fModel
                    in
                    case out of
                        Finder.Remain ->
                            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fc )

                        Finder.Exit ->
                            ( { model | modal = NoModal }, Cmd.none )

                        Finder.OpenDefinition ref ->
                            navigateToDefinition { model | modal = NoModal } ref

                _ ->
                    ( model, Cmd.none )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )



-- UPDATE HELPERS


navigateToDefinition : Model -> Reference -> ( Model, Cmd Msg )
navigateToDefinition model ref =
    ( model, Route.navigateToDefinition model.env.navKey model.route ref )


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
            Route.replacePerspective model.env.navKey (Perspective.toParams perspective) focusedReferenceRoute
    in
    ( { model | workspace = workspace }, changeRouteCmd )


fetchPerspectiveAndCodebaseTree : Perspective -> Model -> ( Model, Cmd Msg )
fetchPerspectiveAndCodebaseTree oldPerspective ({ env } as model) =
    let
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpointUrl model.env

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init codebaseConfig

        fetchNamespaceDetailsCmd =
            env.perspective
                |> fetchNamespaceDetails
                |> Maybe.map (HttpApi.perform env.apiBasePath)
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
handleWorkspaceOutMsg ({ env } as model) out =
    case out of
        Workspace.None ->
            ( model, Cmd.none )

        Workspace.ShowFinderRequest withinNamespace ->
            showFinder model withinNamespace

        Workspace.Focused ref ->
            ( model, Route.navigateToDefinition env.navKey model.route ref )

        Workspace.Emptied ->
            ( model, Route.navigateToCurrentPerspective env.navKey model.route )

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
        KeyboardShortcut.Chord Ctrl (B _) ->
            toggleSidebar

        KeyboardShortcut.Chord Meta (B _) ->
            toggleSidebar

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
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpointUrl model.env

        options =
            SearchOptions.init model.env.perspective withinNamespace

        ( fm, fcmd ) =
            Finder.init codebaseConfig options
    in
    ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fcmd )



-- EFFECTS


fetchNamespaceDetails : Perspective -> Maybe (HttpApi.ApiRequest NamespaceDetails Msg)
fetchNamespaceDetails perspective =
    case perspective of
        Namespace { fqn } ->
            fqn
                |> LocalApi.namespace perspective
                |> HttpApi.toRequest Namespace.decodeDetails (FetchPerspectiveNamespaceDetailsFinished fqn)
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


appTitle : Click msg -> AppHeader.AppTitle msg
appTitle click =
    AppHeader.AppTitle click
        (h1 []
            [ text "Unison"
            , span [ class "context unison-local" ] [ text "Local" ]
            ]
        )


appHeader : AppHeader.AppHeader Msg
appHeader =
    { menuToggle = Just ToggleSidebar
    , appTitle = appTitle (Click.Href "/")
    , banner = Nothing
    , rightButton = Just (Button.button (ShowModal PublishModal) "Publish on Unison Share" |> Button.share)
    }


viewSidebarHeader : Env -> Html Msg
viewSidebarHeader env =
    case env.perspective of
        Codebase _ ->
            UI.nothing

        Namespace { fqn, details } ->
            let
                -- Imprecise, but close enough, approximation of overflowing,
                -- which results in a slight faded left edge A better way would
                -- be to measure the DOM like we do for overflowing docs, but
                -- thats quite involved...
                isOverflowing =
                    fqn |> FQN.toString |> String.length |> (\l -> l > 20)

                hashvatar =
                    details
                        |> RemoteData.map (Namespace.hash >> Hashvatar.view)
                        |> RemoteData.withDefault Hashvatar.empty
            in
            Sidebar.header
                [ Sidebar.headerItem
                    [ classList [ ( "is-overflowing", isOverflowing ) ] ]
                    [ hashvatar
                    , h2 [ class "namespace" ] [ FQN.view fqn ]
                    ]
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
    , ( "Report a bug", OnClick (ShowModal ReportBugModal) )
    , ( "Unison Share", ExternalHref "https://share.unison-lang.org" )
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
    [ viewMainSidebarCollapseButton model
    , div [ class "expanded-content" ]
        [ viewSidebarHeader model.env
        , div [ class "sidebar-scroll-area" ]
            [ Sidebar.section
                "Namespaces and Definitions"
                [ Html.map CodebaseTreeMsg (CodebaseTree.view model.codebaseTree) ]
            , nav []
                (List.map
                    (\( l, c ) -> Click.view [] [ text l ] c)
                    subMenu
                    ++ [ a [ class "show-keyboard-shortcuts", onClick (ShowModal HelpModal) ]
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


viewReportBugModal : Html Msg
viewReportBugModal =
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
                            , strong [] [ text "Unison Local" ]
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
            viewReportBugModal


viewAppLoading : Html msg
viewAppLoading =
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Click.Disabled))
        , PageLayout.view
            (PageLayout.SidebarLayout
                { sidebar = []
                , sidebarToggled = False
                , content = PageLayout.PageContent []
                }
            )
        ]


viewAppError : Http.Error -> Html msg
viewAppError error =
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Click.Disabled))
        , PageLayout.view
            (PageLayout.SidebarLayout
                { sidebar = []
                , sidebarToggled = False
                , content =
                    PageLayout.PageContent
                        [ div [ class "app-error" ]
                            [ Icon.view Icon.warn
                            , p [ title (Util.httpErrorToString error) ]
                                [ text "Unison Local could not be started." ]
                            ]
                        ]
                }
            )
        ]


view : Model -> Browser.Document Msg
view model =
    let
        pageContent =
            case model.route of
                Route.Perspective _ ->
                    Html.map PerspectiveLandingMsg
                        (PerspectiveLanding.view
                            model.env.perspective
                            model.perspectiveLanding
                        )

                Route.Definition _ _ ->
                    Html.map WorkspaceMsg (Workspace.view model.workspace)

        page =
            PageLayout.SidebarLayout
                { sidebar = viewMainSidebar model
                , sidebarToggled = model.sidebarToggled
                , content = PageLayout.PageContent [ pageContent ]
                }
    in
    { title = "Unison Local"
    , body =
        [ div [ id "app" ]
            [ AppHeader.view appHeader
            , PageLayout.view page
            , viewModal model
            ]
        ]
    }
