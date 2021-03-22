module Finder exposing (Model, Msg, OutMsg(..), init, update, view)

import Browser.Dom as Dom
import Definition exposing (Definition)
import FullyQualifiedName as FQN
import Hash exposing (Hash)
import Html exposing (Html, a, div, header, input, label, li, ol, section, text)
import Html.Attributes exposing (autocomplete, class, classList, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onMouseOver)
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key exposing (Key(..))
import KeyboardShortcuts
import List.Nonempty as NEL
import RemoteData exposing (RemoteData(..), WebData)
import SearchResults exposing (SearchResults(..))
import Source exposing (TypeSource(..))
import Task
import UI
import UI.Icon as Icon
import UI.Modal as Modal



-- MODEL


type alias FinderSearchResults =
    SearchResults Definition


type alias Model =
    { query : String, results : WebData FinderSearchResults }


init : ( Model, Cmd Msg )
init =
    let
        mockedResults =
            SearchResults.fromList
                [ Definition.Type (Hash.fromString "#base.List")
                    (Definition.makeTypeDefinitionInfo
                        "List"
                        (NEL.fromElement (FQN.fromString "base.List"))
                        BuiltinType
                    )
                , Definition.Type (Hash.fromString "#base.Map")
                    (Definition.makeTypeDefinitionInfo
                        "Map"
                        (NEL.fromElement (FQN.fromString "base.Map"))
                        BuiltinType
                    )
                , Definition.Type (Hash.fromString "#base.Set")
                    (Definition.makeTypeDefinitionInfo
                        "Set"
                        (NEL.fromElement (FQN.fromString "base.Set"))
                        BuiltinType
                    )
                , Definition.Type (Hash.fromString "#base.List.Nonempty")
                    (Definition.makeTypeDefinitionInfo
                        "List.Nonempty"
                        (NEL.fromElement (FQN.fromString "base.List.Nonempty"))
                        BuiltinType
                    )
                ]
    in
    ( { query = "", results = Success mockedResults }, focusSearchInput )



-- UPDATE


type Msg
    = NoOp
    | UpdateQuery String
    | ResetOrClose
    | Close
    | FocusOn Hash
    | Select Hash
    | Keydown KeyboardEvent


type OutMsg
    = Remain
    | Exit
    | OpenDefinition Hash


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    let
        exit =
            ( model, Cmd.none, Exit )

        reset =
            ( { model | query = "" }, focusSearchInput, Remain )

        resetOrClose =
            if model.query == "" then
                exit

            else
                reset
    in
    case msg of
        UpdateQuery query ->
            ( { model | query = query }, Cmd.none, Remain )

        Close ->
            exit

        ResetOrClose ->
            resetOrClose

        FocusOn hash ->
            let
                definitionEqs =
                    Definition.hash >> Hash.equals hash

                results =
                    model.results
                        |> RemoteData.map (SearchResults.focusOn definitionEqs)
            in
            ( { model | results = results }, Cmd.none, Remain )

        Select hash ->
            ( model, Cmd.none, OpenDefinition hash )

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
                                    OpenDefinition ((SearchResults.focus >> Definition.hash) matches)

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


focusSearchInput : Cmd Msg
focusSearchInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "search")



-- VIEW


viewMatch : String -> Definition -> Bool -> Maybe String -> Html Msg
viewMatch nameWidth def isFocused shortcut =
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

        viewMatch_ hash info source =
            li
                [ classList
                    [ ( "definition-match", True )
                    , ( "focused", isFocused )
                    ]
                , onMouseOver (FocusOn hash)
                , onClick (Select hash)
                ]
                [ Icon.view Icon.Type
                , label [ class "name", style "width" nameWidth ] [ text info.name ]
                , source
                , shortcutIndicator
                ]
    in
    case def of
        Definition.Type hash info ->
            viewMatch_
                hash
                info
                (Source.viewTypeSource (\_ -> NoOp) info.source)

        Definition.Term hash info ->
            viewMatch_
                hash
                info
                (Source.viewTermSource (\_ -> NoOp) info.name info.source)


maxNumNameChars : SearchResults.Matches Definition -> Int
maxNumNameChars matches =
    matches
        |> SearchResults.matchesToList
        |> List.map (Definition.name >> String.length)
        |> List.maximum
        |> Maybe.withDefault 0


viewMatches : SearchResults.Matches Definition -> Html Msg
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
    section [ class "results" ]
        [ ol [] matchItems
        , div [ class "column-line", style "left" nameWidth ] []
        ]


view : Model -> Html Msg
view model =
    let
        results =
            case model.results of
                Success res ->
                    case res of
                        Empty ->
                            UI.emptyStateMessage "Could not find any matches"

                        SearchResults matches ->
                            viewMatches matches

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
