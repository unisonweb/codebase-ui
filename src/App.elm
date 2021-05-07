module App exposing (..)

import Browser
import Browser.Navigation as Nav
import CodebaseTree
import Definition.Reference exposing (Reference(..))
import Finder
import HashQualified exposing (HashQualified(..))
import Html exposing (Html, a, aside, div, h1, h3, header, nav, section, span, text)
import Html.Attributes exposing (class, href, id, target)
import Html.Events exposing (onClick)
import KeyboardShortcut
import KeyboardShortcut.Key as Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import RelativeTo exposing (RelativeTo(..))
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import System exposing (OperatingSystem(..), System)
import UI
import UI.Icon as Icon
import UI.Modal as Modal
import Url exposing (Url)
import Workspace



-- MODEL


type Modal
    = NoModal
    | FinderModal Finder.Model
    | HelpModal


type alias Model =
    { navKey : Nav.Key
    , route : Route
    , relativeTo : RelativeTo
    , codebaseTree : CodebaseTree.Model
    , workspace : Workspace.Model
    , modal : Modal
    , keyboardShortcut : KeyboardShortcut.Model
    , system : System
    }


type alias Flags =
    { system : { operatingSystem : String } }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        route =
            Route.fromUrl url

        ( workspace, workspaceCmd ) =
            case route of
                Route.ByReference _ ref ->
                    Workspace.init (Just ref)

                _ ->
                    Workspace.init Nothing

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init

        {--| TODO: When we can't get a relative to, we need to resolve the name
          in the url to a hash-}
        relativeTo =
            Maybe.withDefault Codebase (Route.relativeTo route)

        system =
            System.fromRecord flags.system

        model =
            { navKey = navKey
            , route = route
            , relativeTo = relativeTo
            , workspace = workspace
            , codebaseTree = codebaseTree
            , modal = NoModal
            , keyboardShortcut = KeyboardShortcut.init system.operatingSystem
            , system = system
            }
    in
    ( model
    , Cmd.batch [ Cmd.map CodebaseTreeMsg codebaseTreeCmd, Cmd.map WorkspaceMsg workspaceCmd ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Keydown KeyboardEvent
    | OpenDefinition Reference
    | ShowHelpModal
    | CloseModal
      -- sub msgs
    | FinderMsg Finder.Msg
    | WorkspaceMsg Workspace.Msg
    | CodebaseTreeMsg CodebaseTree.Msg
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

        OpenDefinition ref ->
            openDefinition model ref

        ShowHelpModal ->
            ( { model | modal = HelpModal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        -- Sub msgs
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

        CodebaseTreeMsg cMsg ->
            let
                ( codebaseTree, cCmd, outMsg ) =
                    CodebaseTree.update cMsg model.codebaseTree

                model2 =
                    { model | codebaseTree = codebaseTree }

                ( model3, cmd ) =
                    case outMsg of
                        CodebaseTree.None ->
                            ( model2, Cmd.none )

                        CodebaseTree.OpenDefinition ref ->
                            openDefinition model2 ref
            in
            ( model3, Cmd.batch [ cmd, Cmd.map CodebaseTreeMsg cCmd ] )

        FinderMsg fMsg ->
            case model.modal of
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

        noOp =
            ( model, Cmd.none )
    in
    case shortcut of
        KeyboardShortcut.Chord Ctrl (K _) ->
            showFinder model

        KeyboardShortcut.Chord Meta (K _) ->
            if model.system.operatingSystem == System.MacOS then
                showFinder model

            else
                noOp

        KeyboardShortcut.Sequence _ ForwardSlash ->
            showFinder model

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
    { m | system : System, modal : Modal }
    -> ( { m | system : System, modal : Modal }, Cmd Msg )
showFinder model =
    let
        ( fm, fcmd ) =
            Finder.init model.system
    in
    ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fcmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
        , Sub.map WorkspaceMsg (Workspace.subscriptions model.workspace)
        ]



-- VIEW


viewMainSidebar : Model -> Html Msg
viewMainSidebar model =
    aside
        [ id "main-sidebar" ]
        [ header [] [ h1 [] [ text "~/.unison" ] ]
        , div [] [ Html.map CodebaseTreeMsg (CodebaseTree.view model.codebaseTree) ]
        , nav []
            [ a [ href "https://unison-lang.org", target "_blank" ] [ Icon.view Icon.UnisonMark ]
            , a [ href "https://unison-lang.org/docs", target "_blank" ] [ text "Docs" ]
            , a [ href "https://unison-lang.org/docs/language-reference", target "_blank" ] [ text "Language Reference" ]
            , a [ href "https://unison-lang.org/community", target "_blank" ] [ text "Community" ]
            , a [ class "show-help", onClick ShowHelpModal ] [ text "Keyboard Shortcuts", KeyboardShortcut.view model.keyboardShortcut (KeyboardShortcut.single QuestionMark) ]
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

        content =
            Modal.Content
                (section
                    [ class "shortcuts" ]
                    [ div [ class "shortcut-group" ]
                        [ h3 [] [ text "General" ]
                        , viewInstructions (span [] [ text "Keyboard shortcuts", UI.subtle " (this dialog)" ]) [ KeyboardShortcut.single QuestionMark ]
                        , viewInstructions (text "Open Finder") openFinderInstructions
                        , viewInstructions (text "Move focus up") [ KeyboardShortcut.single ArrowUp, KeyboardShortcut.single (K Key.Lower) ]
                        , viewInstructions (text "Move focus down") [ KeyboardShortcut.single ArrowDown, KeyboardShortcut.single (J Key.Lower) ]
                        , viewInstructions (text "Close focused definition") [ KeyboardShortcut.single (X Key.Lower) ]
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


viewModal :
    { m | system : System, modal : Modal, keyboardShortcut : KeyboardShortcut.Model }
    -> Html Msg
viewModal model =
    case model.modal of
        NoModal ->
            UI.nothing

        FinderModal m ->
            Html.map FinderMsg (Finder.view m)

        HelpModal ->
            viewHelpModal model.system.operatingSystem model.keyboardShortcut


view : Model -> Browser.Document Msg
view model =
    { title = "Unison Codebase"
    , body =
        [ div [ id "app" ]
            [ viewMainSidebar model
            , Html.map WorkspaceMsg (Workspace.view model.workspace)
            , viewModal model
            ]
        ]
    }
