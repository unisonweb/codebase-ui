module Finder exposing (Model, Msg, OutMsg(..), init, update, view)

import Browser.Dom as Dom
import Definition exposing (Definition)
import Html exposing (Html, a, header, input)
import Html.Attributes exposing (autocomplete, class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key exposing (Key(..))
import RemoteData exposing (WebData)
import SearchResults exposing (SearchResults)
import Task
import UI.Icon as Icon
import UI.Modal as Modal



-- MODEL


type alias FinderSearchResults =
    SearchResults Definition


type alias Model =
    { query : String, results : WebData FinderSearchResults }


init : ( Model, Cmd Msg )
init =
    ( { query = "", results = RemoteData.NotAsked }, focusSearchInput )



-- UPDATE


type Msg
    = NoOp
    | UpdateQuery String
    | ResetOrClose
    | Close
    | HandleKeyboardEvent KeyboardEvent


type OutMsg
    = Remain
    | Exit


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

        HandleKeyboardEvent event ->
            case event.keyCode of
                Escape ->
                    resetOrClose

                _ ->
                    ( model, Cmd.none, Remain )

        _ ->
            ( model, Cmd.none, Remain )



-- EFFECTS


focusSearchInput : Cmd Msg
focusSearchInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "search")



-- VIEW


view : Model -> Html Msg
view model =
    Modal.view
        Close
        [ id "finder"
        , onKeydown
        ]
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
        ]


onKeydown : Html.Attribute Msg
onKeydown =
    let
        decodeKey =
            Decode.map HandleKeyboardEvent decodeKeyboardEvent
    in
    Html.Events.custom "keydown"
        (decodeKey
            |> Decode.andThen
                (\msg ->
                    Decode.succeed
                        { message = msg
                        , stopPropagation = True
                        , preventDefault = False
                        }
                )
        )
