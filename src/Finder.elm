module Finder exposing (Model, Msg, OutMsg(..), init, update, view)

import Api
import Browser.Dom as Dom
import Definition.Category as Category
import Definition.Source as Source
import Definition.Term exposing (Term(..))
import Definition.Type exposing (Type(..))
import Finder.FinderMatch as FinderMatch exposing (FinderMatch)
import HashQualified exposing (HashQualified(..))
import Html
    exposing
        ( Html
        , a
        , div
        , header
        , input
        , label
        , li
        , mark
        , ol
        , section
        , span
        , text
        )
import Html.Attributes
    exposing
        ( autocomplete
        , class
        , classList
        , id
        , placeholder
        , spellcheck
        , style
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import KeyboardShortcut exposing (KeyboardShortcut(..))
import KeyboardShortcut.Key as Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import List.Nonempty as NEL
import RemoteData exposing (RemoteData(..), WebData)
import SearchResults exposing (SearchResults(..))
import Syntax
import Task
import UI
import UI.Icon as Icon
import UI.Modal as Modal
import Workspace.Reference exposing (Reference(..))



-- MODEL


type alias FinderSearchResults =
    SearchResults FinderMatch


type alias Model =
    { query : String
    , results : WebData FinderSearchResults
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { query = ""
      , results = NotAsked
      , keyboardShortcut = KeyboardShortcut.init
      }
    , focusSearchInput
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateQuery String
    | ResetOrClose
    | Close
    | Select Reference
    | Keydown KeyboardEvent
    | FetchMatchesFinished String (WebData (List FinderMatch))
    | KeyboardShortcutMsg KeyboardShortcut.Msg


type OutMsg
    = Remain
    | Exit
    | OpenDefinition Reference


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    let
        exit =
            ( model, Cmd.none, Exit )

        reset =
            ( { model | query = "", results = NotAsked }, focusSearchInput, Remain )

        resetOrClose =
            if model.query == "" then
                exit

            else
                reset
    in
    case msg of
        NoOp ->
            ( model, Cmd.none, Remain )

        UpdateQuery query ->
            let
                isSequenceShortcutInput =
                    String.contains ";" query
            in
            if String.isEmpty query then
                ( { model | query = query, results = NotAsked }, Cmd.none, Remain )

            else if String.length query > 1 && not isSequenceShortcutInput then
                ( { model | query = query }, fetchMatches query, Remain )

            else if not isSequenceShortcutInput then
                ( { model | query = query }, Cmd.none, Remain )

            else
                ( model, Cmd.none, Remain )

        Close ->
            exit

        FetchMatchesFinished query matches ->
            let
                results =
                    RemoteData.map SearchResults.fromList matches
            in
            if query == model.query then
                ( { model | results = results }, Cmd.none, Remain )

            else
                ( model, Cmd.none, Remain )

        ResetOrClose ->
            resetOrClose

        Select ref ->
            ( model, Cmd.none, OpenDefinition ref )

        Keydown event ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect model.keyboardShortcut event.key

                cmd =
                    Cmd.map KeyboardShortcutMsg kCmd

                newModel =
                    { model | keyboardShortcut = keyboardShortcut }

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event
            in
            case shortcut of
                Sequence _ Escape ->
                    resetOrClose

                Sequence _ ArrowUp ->
                    let
                        newResults =
                            RemoteData.map SearchResults.prev model.results
                    in
                    ( { newModel | results = newResults }, cmd, Remain )

                Sequence _ ArrowDown ->
                    let
                        newResults =
                            RemoteData.map SearchResults.next model.results
                    in
                    ( { newModel | results = newResults }, cmd, Remain )

                Sequence _ Enter ->
                    let
                        openFocused results =
                            case results of
                                Empty ->
                                    Remain

                                SearchResults matches ->
                                    OpenDefinition ((SearchResults.focus >> FinderMatch.reference) matches)

                        out =
                            model.results
                                |> RemoteData.map openFocused
                                |> RemoteData.withDefault Remain
                    in
                    ( newModel, cmd, out )

                Sequence (Just Semicolon) k ->
                    case Key.toNumber k of
                        Just n ->
                            let
                                out =
                                    model.results
                                        |> RemoteData.toMaybe
                                        |> Maybe.andThen (SearchResults.getAt (n - 1))
                                        |> Maybe.map FinderMatch.reference
                                        |> Maybe.map OpenDefinition
                                        |> Maybe.withDefault Remain
                            in
                            ( newModel, cmd, out )

                        Nothing ->
                            ( newModel, cmd, Remain )

                _ ->
                    ( newModel, cmd, Remain )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd, Remain )



-- EFFECTS


fetchMatches : String -> Cmd Msg
fetchMatches query =
    let
        limit =
            9

        sourceWidth =
            Syntax.Width 100
    in
    Http.get
        { url = Api.find limit sourceWidth query
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> FetchMatchesFinished query)
                FinderMatch.decodeMatches
        }


focusSearchInput : Cmd Msg
focusSearchInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "search")



-- VIEW


indexToShortcut : Int -> Maybe Key
indexToShortcut index =
    let
        n =
            index + 1
    in
    if n > 9 then
        Nothing

    else
        n |> String.fromInt |> Key.fromString |> Just


viewMarkedNaming : String -> FinderMatch.MatchPositions -> Maybe String -> String -> Html msg
viewMarkedNaming nameWidth matchedPositions namespace name =
    let
        namespaceMod =
            namespace
                |> Maybe.map String.length
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0

        mark_ mod i s =
            if NEL.member (i + mod) matchedPositions then
                mark [] [ text s ]

            else
                text s

        markedName =
            name
                |> String.toList
                |> List.map (List.singleton >> String.fromList)
                |> List.indexedMap (mark_ namespaceMod)

        markedNamespace =
            namespace
                |> Maybe.map String.toList
                |> Maybe.map (List.map (List.singleton >> String.fromList))
                |> Maybe.map (List.indexedMap (mark_ 0))
                |> Maybe.map (\ns -> span [ class "in" ] [ text "in" ] :: ns)
                |> Maybe.withDefault []
    in
    div
        [ class "naming", style "width" nameWidth ]
        [ label [ class "name" ] markedName
        , label [ class "namespace" ] markedNamespace
        ]


viewMatch : KeyboardShortcut.Model -> String -> FinderMatch -> Bool -> Maybe Key -> Html Msg
viewMatch keyboardShortcut nameWidth match isFocused shortcut =
    let
        shortcutIndicator =
            if isFocused then
                KeyboardShortcut.viewShortcut keyboardShortcut (Sequence Nothing Key.Enter)

            else
                case shortcut of
                    Nothing ->
                        UI.nothing

                    Just key ->
                        KeyboardShortcut.viewShortcut keyboardShortcut (Sequence (Just Key.Semicolon) key)

        viewMatch_ reference category naming source =
            li
                [ classList [ ( "definition-match", True ), ( "focused", isFocused ) ]
                , onClick (Select reference)
                ]
                [ Icon.view (Category.icon category)
                , naming
                , div [ class "source" ] [ source ]
                , shortcutIndicator
                ]
    in
    case match.item of
        FinderMatch.TypeItem (Type hash category { name, namespace, source }) ->
            viewMatch_
                (TypeReference (HashOnly hash))
                (Category.Type category)
                (viewMarkedNaming nameWidth match.matchPositions namespace name)
                (Source.viewTypeSource Source.Monochrome source)

        FinderMatch.TermItem (Term hash category { name, namespace, signature }) ->
            viewMatch_
                (TermReference (HashOnly hash))
                (Category.Term category)
                (viewMarkedNaming nameWidth match.matchPositions namespace name)
                (Source.viewTermSignature Source.Monochrome signature)


maxNumNameChars : SearchResults.Matches FinderMatch -> Int
maxNumNameChars matches =
    let
        nameWidth =
            FinderMatch.name >> String.length

        namespaceWidth =
            FinderMatch.namespace >> Maybe.map String.length >> Maybe.withDefault 0

        nameLengthOrNamespaceLength fm =
            max (nameWidth fm) (namespaceWidth fm)
    in
    matches
        |> SearchResults.matchesToList
        |> List.map nameLengthOrNamespaceLength
        |> List.maximum
        |> Maybe.withDefault 0


viewMatches : KeyboardShortcut.Model -> SearchResults.Matches FinderMatch -> Html Msg
viewMatches keyboardShortcut matches =
    let
        nameWidth =
            UI.charWidth (maxNumNameChars matches)

        matchItems =
            matches
                |> SearchResults.mapMatchesToList (\d f -> ( d, f ))
                |> List.indexedMap (\i ( d, f ) -> ( d, f, indexToShortcut i ))
                |> List.map (\( d, f, s ) -> viewMatch keyboardShortcut nameWidth d f s)
    in
    section [ class "results" ] [ ol [] matchItems ]


view : Model -> Html Msg
view model =
    let
        results =
            case model.results of
                Success res ->
                    case res of
                        Empty ->
                            UI.emptyStateMessage ("No matching definitions found for '" ++ model.query ++ "'")

                        SearchResults matches ->
                            viewMatches model.keyboardShortcut matches

                Failure error ->
                    div [] [ text (Api.errorToString error) ]

                _ ->
                    UI.nothing
    in
    -- We stopPropagation such that movement shortcuts, like J or K, for the
    -- workspace aren't triggered when in the modal when the use is trying to
    -- type those letters into the search field
    Modal.view
        Close
        [ id "finder", KeyboardEvent.stopPropagationOn KeyboardEvent.Keydown Keydown ]
        [ header []
            [ Icon.view Icon.Search
            , input
                [ type_ "text"
                , id "search"
                , autocomplete False
                , spellcheck False
                , placeholder "Search by name, namespace, and/or type"
                , onInput UpdateQuery
                , value model.query
                ]
                []
            , a [ class "reset", onClick ResetOrClose ] [ Icon.view Icon.X ]
            ]
        , results
        ]
