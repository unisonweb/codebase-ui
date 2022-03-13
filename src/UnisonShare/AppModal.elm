module UnisonShare.AppModal exposing (..)

import Definition.Reference exposing (Reference)
import Env exposing (Env, OperatingSystem(..))
import Finder
import Finder.SearchOptions as SearchOptions
import FullyQualifiedName as FQN exposing (FQN)
import Html exposing (Html, a, div, h3, p, section, span, strong, text)
import Html.Attributes exposing (class, href, rel, target)
import KeyboardShortcut
import KeyboardShortcut.Key as Key exposing (Key(..))
import UI
import UI.Button as Button
import UI.CopyField as CopyField
import UI.Modal as Modal



-- MODEL


type AppModal
    = FinderModal Finder.Model
    | KeyboardShortcutsModal
    | ReportBugModal
    | PublishModal
    | DownloadModal FQN


type Model
    = NoModal
    | Visible AppModal


init : Model
init =
    NoModal



-- UPDATE


type Msg
    = Close
    | FinderMsg Finder.Msg


type OutMsg
    = None
    | OpenDefinition Reference


update : Env -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update env msg model =
    case ( model, msg ) of
        ( Visible (FinderModal fModel), FinderMsg fMsg ) ->
            let
                ( fm, fc, out ) =
                    Finder.update env fMsg fModel
            in
            case out of
                Finder.Remain ->
                    ( Visible (FinderModal fm), Cmd.map FinderMsg fc, None )

                Finder.Exit ->
                    ( close, Cmd.none, None )

                Finder.OpenDefinition ref ->
                    ( close, Cmd.none, OpenDefinition ref )

        ( Visible _, Close ) ->
            ( close, Cmd.none, None )

        _ ->
            ( model, Cmd.none, None )



-- API


show : AppModal -> ( Model, Cmd Msg )
show modal =
    ( Visible modal, Cmd.none )


showFinder : Env -> Maybe FQN -> ( Model, Cmd Msg )
showFinder env withinNamespace =
    let
        options =
            SearchOptions.init env.perspective withinNamespace

        ( fm, fcmd ) =
            Finder.init env options
    in
    ( Visible (FinderModal fm), Cmd.map FinderMsg fcmd )


close : Model
close =
    NoModal


isOpen : Model -> Bool
isOpen model =
    model == NoModal


modalIs : Model -> AppModal -> Bool
modalIs model modal =
    case model of
        NoModal ->
            False

        Visible m ->
            m == modal



-- VIEW


viewDownloadModal : FQN -> Html Msg
viewDownloadModal fqn =
    let
        prettyName =
            FQN.toString fqn

        unqualified =
            FQN.unqualifiedName fqn

        pullCommand =
            "pull https://github.com/unisonweb/share:." ++ prettyName ++ " ." ++ unqualified

        content =
            Modal.Content
                (section
                    []
                    [ p [] [ text "Download ", UI.bold prettyName, text " by pulling the namespace from Unison Share into a namespace in your local codebase:" ]
                    , CopyField.copyField (\_ -> Close) pullCommand |> CopyField.withPrefix ".>" |> CopyField.view
                    , div [ class "hint" ] [ text "Copy and paste this command into UCM." ]
                    ]
                )
    in
    Modal.modal "download-modal" Close content
        |> Modal.withHeader ("Download " ++ prettyName)
        |> Modal.view


viewKeyboardShortcutsModal : OperatingSystem -> KeyboardShortcut.Model -> Html Msg
viewKeyboardShortcutsModal os keyboardShortcut =
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
    Modal.modal "help-modal" Close content
        |> Modal.withHeader "Keyboard shortcuts"
        |> Modal.view


viewPublishModal : Html Msg
viewPublishModal =
    let
        content =
            Modal.Content
                (section
                    []
                    [ p [ class "main" ]
                        [ text "With your Unison codebase on GitHub, open a Pull Request against "
                        , Button.github "unisonweb/share" |> Button.view
                        , text " to list (or unlist) your project on Unison Share."
                        ]
                    , a [ class "help", href "https://www.unisonweb.org/docs/codebase-organization/#day-to-day-development-creating-and-merging-pull-requests", rel "noopener", target "_blank" ] [ text "How do I get my code on GitHub?" ]
                    ]
                )
    in
    Modal.modal "publish-modal" Close content
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
                            [ Button.github "unisonweb/codebase-ui" |> Button.view
                            , text "for reports on"
                            , strong [] [ text "Unison Share" ]
                            , span [ class "subtle" ] [ text "(this UI)" ]
                            ]
                        , div [ class "action" ]
                            [ Button.github "unisonweb/unison" |> Button.view
                            , text "for reports on the"
                            , strong [] [ text "Unison Language" ]
                            , span [ class "subtle" ] [ text "(UCM)" ]
                            ]
                        ]
                    ]
                )
    in
    Modal.modal "report-bug-modal" Close content
        |> Modal.withHeader "Report a Bug"
        |> Modal.view


view : Env -> Model -> Html Msg
view env model =
    case model of
        NoModal ->
            UI.nothing

        Visible (FinderModal m) ->
            Html.map FinderMsg (Finder.view m)

        Visible KeyboardShortcutsModal ->
            viewKeyboardShortcutsModal
                env.operatingSystem
                (KeyboardShortcut.init env.operatingSystem)

        Visible PublishModal ->
            viewPublishModal

        Visible ReportBugModal ->
            viewReportBugModal

        Visible (DownloadModal fqn) ->
            viewDownloadModal fqn
