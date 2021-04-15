module Finder exposing (Model, Msg, OutMsg(..), init, update, view)

import Api
import Browser.Dom as Dom
import Definition.Category as Category
import Definition.Source as Source
import Definition.Term exposing (Term(..))
import Definition.Type exposing (Type(..))
import Finder.FinderMatch as FinderMatch exposing (FinderMatch)
import HashQualified exposing (HashQualified(..))
import Html exposing (Html, a, div, header, input, label, li, ol, section, text)
import Html.Attributes exposing (autocomplete, class, classList, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key exposing (Key(..))
import KeyboardShortcuts
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
    { query : String, results : WebData FinderSearchResults }


init : ( Model, Cmd Msg )
init =
    ( { query = "", results = NotAsked }, focusSearchInput )



-- UPDATE


type Msg
    = NoOp
    | UpdateQuery String
    | ResetOrClose
    | Close
    | Select Reference
    | Keydown KeyboardEvent
    | FetchMatchesFinished String (WebData (List FinderMatch))


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
        UpdateQuery query ->
            if String.isEmpty query then
                ( { model | query = query, results = NotAsked }, Cmd.none, Remain )

            else if String.length query > 1 then
                ( { model | query = query }, fetchMatches query, Remain )

            else
                ( { model | query = query }, Cmd.none, Remain )

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
            case event.keyCode of
                Escape ->
                    resetOrClose

                Up ->
                    let
                        newResults =
                            RemoteData.map SearchResults.prev model.results
                    in
                    ( { model | results = newResults }, Cmd.none, Remain )

                Down ->
                    let
                        newResults =
                            RemoteData.map SearchResults.next model.results
                    in
                    ( { model | results = newResults }, Cmd.none, Remain )

                Enter ->
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
                    ( model, Cmd.none, out )

                _ ->
                    ( model, Cmd.none, Remain )

        _ ->
            ( model, Cmd.none, Remain )



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


viewMatch : String -> FinderMatch -> Bool -> Maybe String -> Html Msg
viewMatch nameWidth match isFocused shortcut =
    let
        shortcutIndicator =
            if isFocused then
                KeyboardShortcuts.viewShortcut (KeyboardShortcuts.Single "↵ ")

            else
                case shortcut of
                    Nothing ->
                        UI.nothing

                    Just key ->
                        KeyboardShortcuts.viewShortcut (KeyboardShortcuts.Sequence ";" key)

        viewMatch_ reference category name source =
            li
                [ classList
                    [ ( "definition-match", True )
                    , ( "focused", isFocused )
                    ]
                , onClick (Select reference)
                ]
                [ Icon.view (Category.icon category)
                , label [ class "name", style "width" nameWidth ] [ text name ]
                , div [ class "source" ] [ source ]
                , shortcutIndicator
                ]
    in
    case match.item of
        FinderMatch.TypeItem (Type hash category { name, source }) ->
            viewMatch_
                (TypeReference (HashOnly hash))
                (Category.Type category)
                name
                (Source.viewTypeSource Source.Monochrome source)

        -- (Source.viewTypeSource Source.Monochrome info.source)
        FinderMatch.TermItem (Term hash category { name, signature }) ->
            viewMatch_
                (TermReference (HashOnly hash))
                (Category.Term category)
                name
                (Source.viewTermSignature Source.Monochrome signature)


maxNumNameChars : SearchResults.Matches FinderMatch -> Int
maxNumNameChars matches =
    matches
        |> SearchResults.matchesToList
        |> List.map (FinderMatch.name >> String.length)
        |> List.maximum
        |> Maybe.withDefault 0


viewMatches : SearchResults.Matches FinderMatch -> Html Msg
viewMatches matches =
    let
        nameWidth =
            UI.charWidth (maxNumNameChars matches)

        matchItems =
            matches
                |> SearchResults.mapMatchesToList (\d f -> ( d, f ))
                |> List.indexedMap (\i ( d, f ) -> ( d, f, KeyboardShortcuts.indexToShortcut i ))
                |> List.map (\( d, f, s ) -> viewMatch nameWidth d f s)
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
                            viewMatches matches

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
        [ id "finder", KeyboardShortcuts.stopPropagationOnKeydown Keydown ]
        [ header []
            [ Icon.view Icon.Search
            , input
                [ type_ "text"
                , id "search"
                , autocomplete False
                , placeholder "Search by name, namespace, and/or type"
                , onInput UpdateQuery
                , value model.query
                ]
                []
            , a [ class "reset", onClick ResetOrClose ] [ Icon.view Icon.X ]
            ]
        , results
        ]
