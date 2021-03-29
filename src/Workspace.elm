module Workspace exposing (Model, Msg, OutMsg(..), init, open, subscriptions, update, view)

import Api
import Browser.Dom as Dom
import Browser.Events
import Definition exposing (Definition)
import Hash exposing (Hash)
import HashQualified exposing (HashQualified)
import Html
    exposing
        ( Html
        , article
        , header
        , section
        )
import Html.Attributes exposing (class, id)
import Http
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key exposing (Key(..))
import OpenDefinitions exposing (HashIndexedDefinition, OpenDefinitions)
import RemoteData exposing (RemoteData(..), WebData)
import Task
import UI
import UI.Button as Button


type alias Model =
    OpenDefinitions


init : ( Model, Cmd Msg )
init =
    ( OpenDefinitions.init Nothing, Cmd.none )


type Msg
    = NoOp
    | Find
    | OpenDefinitionAfter Hash Hash
    | CloseDefinition Hash
    | FetchOpenDefinitionFinished HashQualified (WebData Definition)
    | HandleKeyboardEvent KeyboardEvent


type OutMsg
    = None
    | ShowFinderRequest


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, None )

        Find ->
            ( model, Cmd.none, ShowFinderRequest )

        OpenDefinitionAfter afterHash hash ->
            let
                ( newModel, cmd ) =
                    openDefinition model (Just afterHash) (HashQualified.HashOnly hash)
            in
            ( newModel, cmd, None )

        CloseDefinition hash ->
            ( OpenDefinitions.remove hash model, Cmd.none, None )

        FetchOpenDefinitionFinished hq response ->
            let
                hash =
                    HashQualified.hash hq

                nextOpenDefinitions =
                    OpenDefinitions.replace hash response model
            in
            ( nextOpenDefinitions, Cmd.none, None )

        HandleKeyboardEvent event ->
            let
                ( newModel, cmd ) =
                    handleKeyboardEvent model event
            in
            ( newModel, cmd, None )


open : Model -> HashQualified -> ( Model, Cmd Msg )
open model hiq =
    openDefinition model Nothing hiq


openDefinition : Model -> Maybe Hash -> HashQualified -> ( Model, Cmd Msg )
openDefinition model afterHash hq =
    let
        hash =
            HashQualified.hash hq
    in
    -- We don't want to refetch or replace any already open definitions, but we
    -- do want to focus and scroll to it
    if OpenDefinitions.member hash model then
        let
            nextOpenDefinitions =
                OpenDefinitions.focusOn hash model
        in
        ( nextOpenDefinitions, scrollToDefinition hash )

    else
        let
            toInsert =
                HashIndexedDefinition hash Loading

            insert =
                case afterHash of
                    Nothing ->
                        OpenDefinitions.insertWithFocus toInsert

                    Just h ->
                        OpenDefinitions.insertWithFocusAfter h toInsert

            nextOpenDefinitions =
                insert model
        in
        ( nextOpenDefinitions
        , Cmd.batch [ fetchDefinition hq, scrollToDefinition hash ]
        )


handleKeyboardEvent : Model -> KeyboardEvent -> ( Model, Cmd Msg )
handleKeyboardEvent model keyboardEvent =
    let
        scrollToCmd =
            OpenDefinitions.focus
                >> Maybe.map .hash
                >> Maybe.map scrollToDefinition
                >> Maybe.withDefault Cmd.none

        nextDefinition =
            let
                newOpenDefinitions =
                    OpenDefinitions.next model
            in
            ( newOpenDefinitions, scrollToCmd newOpenDefinitions )

        prevDefinitions =
            let
                newOpenDefinitions =
                    OpenDefinitions.prev model
            in
            ( newOpenDefinitions, scrollToCmd newOpenDefinitions )
    in
    case keyboardEvent.keyCode of
        Down ->
            if keyboardEvent.shiftKey then
                nextDefinition

            else
                ( model, Cmd.none )

        J ->
            if keyboardEvent.shiftKey then
                nextDefinition

            else
                ( model, Cmd.none )

        Up ->
            if keyboardEvent.shiftKey then
                prevDefinitions

            else
                ( model, Cmd.none )

        K ->
            if keyboardEvent.shiftKey then
                prevDefinitions

            else
                ( model, Cmd.none )

        X ->
            let
                newOpenDefinitions =
                    model
                        |> OpenDefinitions.focus
                        |> Maybe.map (\hid -> OpenDefinitions.remove hid.hash model)
                        |> Maybe.withDefault model
            in
            ( newOpenDefinitions, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


fetchDefinition : HashQualified -> Cmd Msg
fetchDefinition hq =
    Http.get
        { url = Api.getDefinition [ HashQualified.toString HashQualified.PreferName hq ]
        , expect =
            Http.expectJson
                (RemoteData.fromResult
                    >> FetchOpenDefinitionFinished hq
                )
                Definition.decodeHead
        }


scrollToDefinition : Hash -> Cmd Msg
scrollToDefinition hash =
    let
        id =
            "definition-" ++ Hash.toString hash
    in
    Task.sequence
        [ Dom.getElement id |> Task.map (.element >> .y)
        , Dom.getElement "workspace-content" |> Task.map (.element >> .y)
        , Dom.getViewportOf "workspace-content" |> Task.map (.viewport >> .y)
        ]
        |> Task.andThen
            (\outcome ->
                case outcome of
                    elY :: viewportY :: viewportScrollTop :: [] ->
                        Dom.setViewportOf "workspace-content" 0 (viewportScrollTop + (elY - viewportY))
                            |> Task.onError (\_ -> Task.succeed ())

                    _ ->
                        Task.succeed ()
            )
        |> Task.attempt (always NoOp)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
        (Decode.map HandleKeyboardEvent
            decodeKeyboardEvent
        )



-- VIEW


view : Model -> Html Msg
view model =
    article [ id "workspace" ]
        [ header
            [ id "workspace-toolbar" ]
            [ Button.secondary Find "Find" ]
        , section
            [ id "workspace-content" ]
            [ section [ class "definitions-pane" ] (viewOpenDefinitions model) ]
        ]


viewDefinition : HashIndexedDefinition -> Bool -> Html Msg
viewDefinition hid isFocused =
    case hid.definition of
        Success def ->
            Definition.view
                (CloseDefinition hid.hash)
                (OpenDefinitionAfter hid.hash)
                def
                isFocused

        Failure err ->
            Definition.viewError
                (CloseDefinition hid.hash)
                hid.hash
                isFocused
                err

        NotAsked ->
            UI.nothing

        Loading ->
            Definition.viewLoading hid.hash isFocused


viewOpenDefinitions : OpenDefinitions -> List (Html Msg)
viewOpenDefinitions =
    OpenDefinitions.mapToList viewDefinition
