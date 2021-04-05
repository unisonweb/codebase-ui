module Workspace exposing (Model, Msg, OutMsg(..), init, open, subscriptions, update, view)

import Api
import Browser.Dom as Dom
import Browser.Events
import Definition exposing (Definition)
import Hash exposing (Hash)
import HashQualified exposing (HashQualified(..))
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
import Route exposing (Route(..))
import Task
import UI
import UI.Button as Button



-- MODEL


type alias Model =
    OpenDefinitions


init : Maybe HashQualified -> ( Model, Cmd Msg )
init definitionHq =
    let
        model =
            OpenDefinitions.init Nothing
    in
    case definitionHq of
        Nothing ->
            ( model, Cmd.none )

        Just hq ->
            let
                ( m, c, _ ) =
                    openDefinition model Nothing hq
            in
            ( m, c )



-- UPDATE


type Msg
    = NoOp
    | Find
    | OpenDefinitionAfter Hash Hash
    | CloseDefinition Hash
    | FetchOpenDefinitionFinished HashQualified (WebData Definition)
    | HandleKeyboardEvent KeyboardEvent


type OutMsg
    = None
    | TermFocused HashQualified
    | TypeFocused HashQualified
    | Emptied
    | ShowFinderRequest


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, None )

        Find ->
            ( model, Cmd.none, ShowFinderRequest )

        OpenDefinitionAfter afterHash hash ->
            openDefinition model (Just afterHash) (HashQualified.HashOnly hash)

        CloseDefinition hash ->
            ( OpenDefinitions.remove hash model, Cmd.none, None )

        FetchOpenDefinitionFinished hq response ->
            case HashQualified.hash hq of
                Just h ->
                    let
                        nextOpenDefinitions =
                            OpenDefinitions.replace h response model
                    in
                    ( nextOpenDefinitions, Cmd.none, openDefinitionsFocusToOutMsg nextOpenDefinitions )

                Nothing ->
                    -- TODO
                    ( model, Cmd.none, None )

        HandleKeyboardEvent event ->
            handleKeyboardEvent model event



-- UPDATE HELPERS


open : Model -> HashQualified -> ( Model, Cmd Msg, OutMsg )
open model hiq =
    openDefinition model Nothing hiq


openDefinition : Model -> Maybe Hash -> HashQualified -> ( Model, Cmd Msg, OutMsg )
openDefinition model afterHash hq =
    case HashQualified.hash hq of
        -- TODO: OpenDefinitions should be indexed by HashQualified instead of Hash
        Nothing ->
            ( model, Cmd.none, None )

        Just hash ->
            -- We don't want to refetch or replace any already open definitions, but we
            -- do want to focus and scroll to it
            if OpenDefinitions.member hash model then
                let
                    nextOpenDefinitions =
                        OpenDefinitions.focusOn hash model
                in
                ( nextOpenDefinitions
                , scrollToDefinition hash
                , openDefinitionsFocusToOutMsg nextOpenDefinitions
                )

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
                , openDefinitionsFocusToOutMsg nextOpenDefinitions
                )


openDefinitionsFocusToOutMsg : OpenDefinitions -> OutMsg
openDefinitionsFocusToOutMsg openDefs =
    let
        toFocusedOut : HashIndexedDefinition -> OutMsg
        toFocusedOut hid =
            case hid.definition of
                Success def ->
                    case def of
                        Definition.Type h _ ->
                            TypeFocused (HashOnly h)

                        Definition.Term h _ ->
                            TermFocused (HashOnly h)

                _ ->
                    None
    in
    openDefs
        |> OpenDefinitions.focus
        |> Maybe.map toFocusedOut
        |> Maybe.withDefault Emptied


handleKeyboardEvent : Model -> KeyboardEvent -> ( Model, Cmd Msg, OutMsg )
handleKeyboardEvent model keyboardEvent =
    let
        scrollToCmd =
            OpenDefinitions.focus
                >> Maybe.map .hash
                >> Maybe.map scrollToDefinition
                >> Maybe.withDefault Cmd.none

        nextDefinition =
            let
                next =
                    OpenDefinitions.next model
            in
            ( next, scrollToCmd next, openDefinitionsFocusToOutMsg next )

        prevDefinitions =
            let
                prev =
                    OpenDefinitions.prev model
            in
            ( prev, scrollToCmd prev, openDefinitionsFocusToOutMsg prev )

        passthrough =
            ( model, Cmd.none, None )
    in
    case keyboardEvent.keyCode of
        Down ->
            if keyboardEvent.shiftKey then
                nextDefinition

            else
                passthrough

        J ->
            if keyboardEvent.shiftKey then
                nextDefinition

            else
                passthrough

        Up ->
            if keyboardEvent.shiftKey then
                prevDefinitions

            else
                passthrough

        K ->
            if keyboardEvent.shiftKey then
                prevDefinitions

            else
                passthrough

        X ->
            let
                without =
                    model
                        |> OpenDefinitions.focus
                        |> Maybe.map (\hid -> OpenDefinitions.remove hid.hash model)
                        |> Maybe.withDefault model
            in
            ( without, Cmd.none, openDefinitionsFocusToOutMsg without )

        _ ->
            passthrough



-- EFFECTS


fetchDefinition : HashQualified -> Cmd Msg
fetchDefinition hq =
    Http.get
        { url = Api.getDefinition [ HashQualified.toString hq ]
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
