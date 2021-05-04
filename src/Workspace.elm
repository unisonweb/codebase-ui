module Workspace exposing (Model, Msg, OutMsg(..), init, open, subscriptions, update, view)

import Api
import Browser.Dom as Dom
import Definition.Reference as Reference exposing (Reference)
import HashQualified exposing (HashQualified(..))
import Html exposing (Html, article, header, section)
import Html.Attributes exposing (class, id)
import Http
import KeyboardShortcut.Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import Route exposing (Route(..))
import Task
import UI
import UI.Button as Button
import Workspace.WorkspaceItem as WorkspaceItem exposing (Item(..), WorkspaceItem(..))
import Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems)



-- MODEL


type alias Model =
    WorkspaceItems


init : Maybe Reference -> ( Model, Cmd Msg )
init mRef =
    let
        model =
            WorkspaceItems.init Nothing
    in
    case mRef of
        Nothing ->
            ( model, Cmd.none )

        Just ref ->
            let
                ( m, c, _ ) =
                    open model ref
            in
            ( m, c )



-- UPDATE


type Msg
    = NoOp
    | Find
    | OpenDefinitionAfter Reference Reference
    | CloseDefinition Reference
    | FetchItemFinished Reference (Result Http.Error Item)
    | Keydown KeyboardEvent


type OutMsg
    = None
    | Focused Reference
    | Emptied
    | ShowFinderRequest


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, None )

        Find ->
            ( model, Cmd.none, ShowFinderRequest )

        OpenDefinitionAfter afterRef ref ->
            openItem model (Just afterRef) ref

        CloseDefinition ref ->
            let
                nextModel =
                    WorkspaceItems.remove model ref
            in
            ( nextModel, Cmd.none, openDefinitionsFocusToOutMsg nextModel )

        FetchItemFinished ref itemResult ->
            let
                workspaceItem =
                    case itemResult of
                        Err e ->
                            WorkspaceItem.Failure ref e

                        Ok i ->
                            WorkspaceItem.Success ref i

                nextWorkspaceItems =
                    WorkspaceItems.replace model ref workspaceItem
            in
            ( nextWorkspaceItems, Cmd.none, openDefinitionsFocusToOutMsg nextWorkspaceItems )

        Keydown event ->
            keydown model event



-- UPDATE HELPERS


open : Model -> Reference -> ( Model, Cmd Msg, OutMsg )
open model ref =
    openItem model Nothing ref


openItem : Model -> Maybe Reference -> Reference -> ( Model, Cmd Msg, OutMsg )
openItem model afterRef ref =
    -- We don't want to refetch or replace any already open definitions, but we
    -- do want to focus and scroll to it
    if WorkspaceItems.member model ref then
        let
            nextWorkspaceItems =
                WorkspaceItems.focusOn model ref
        in
        ( nextWorkspaceItems
        , scrollToDefinition ref
        , openDefinitionsFocusToOutMsg nextWorkspaceItems
        )

    else
        let
            toInsert =
                WorkspaceItem.Loading ref

            nextWorkspaceItems =
                case afterRef of
                    Nothing ->
                        WorkspaceItems.insertWithFocus model toInsert

                    Just r ->
                        WorkspaceItems.insertWithFocusAfter model r toInsert
        in
        ( nextWorkspaceItems
        , Cmd.batch [ fetchDefinition ref, scrollToDefinition ref ]
        , openDefinitionsFocusToOutMsg nextWorkspaceItems
        )


openDefinitionsFocusToOutMsg : WorkspaceItems -> OutMsg
openDefinitionsFocusToOutMsg openDefs =
    let
        toFocusedOut workspaceItem =
            case workspaceItem of
                Success ref _ ->
                    Focused ref

                _ ->
                    None
    in
    openDefs
        |> WorkspaceItems.focus
        |> Maybe.map toFocusedOut
        |> Maybe.withDefault Emptied


keydown : Model -> KeyboardEvent -> ( Model, Cmd Msg, OutMsg )
keydown model keyboardEvent =
    let
        scrollToCmd =
            WorkspaceItems.focus
                >> Maybe.map WorkspaceItem.reference
                >> Maybe.map scrollToDefinition
                >> Maybe.withDefault Cmd.none

        nextDefinition =
            let
                next =
                    WorkspaceItems.next model
            in
            ( next, scrollToCmd next, openDefinitionsFocusToOutMsg next )

        prevDefinitions =
            let
                prev =
                    WorkspaceItems.prev model
            in
            ( prev, scrollToCmd prev, openDefinitionsFocusToOutMsg prev )

        passthrough =
            ( model, Cmd.none, None )
    in
    case keyboardEvent.key of
        ArrowDown ->
            if keyboardEvent.shiftKey then
                nextDefinition

            else
                passthrough

        J _ ->
            if keyboardEvent.shiftKey then
                nextDefinition

            else
                passthrough

        ArrowUp ->
            if keyboardEvent.shiftKey then
                prevDefinitions

            else
                passthrough

        K _ ->
            if keyboardEvent.shiftKey then
                prevDefinitions

            else
                passthrough

        X _ ->
            let
                without =
                    model
                        |> WorkspaceItems.focus
                        |> Maybe.map (WorkspaceItem.reference >> WorkspaceItems.remove model)
                        |> Maybe.withDefault model
            in
            ( without, Cmd.none, openDefinitionsFocusToOutMsg without )

        _ ->
            passthrough



-- EFFECTS


fetchDefinition : Reference -> Cmd Msg
fetchDefinition ref =
    Http.get
        { url = Api.getDefinition [ (Reference.hashQualified >> HashQualified.toString) ref ]
        , expect =
            Http.expectJson
                (FetchItemFinished ref)
                WorkspaceItem.decodeItem
        }


scrollToDefinition : Reference -> Cmd Msg
scrollToDefinition ref =
    let
        id =
            "definition-" ++ Reference.toString ref
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
    KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown



-- VIEW


view : Model -> Html Msg
view model =
    article [ id "workspace" ]
        [ header
            [ id "workspace-toolbar" ]
            [ Button.secondary Find "Find" ]
        , section
            [ id "workspace-content" ]
            [ section [ class "definitions-pane" ] (viewWorkspaceItems model) ]
        ]


viewItem : WorkspaceItem -> Bool -> Html Msg
viewItem workspaceItem isFocused =
    let
        ref =
            WorkspaceItem.reference workspaceItem
    in
    WorkspaceItem.view
        (CloseDefinition ref)
        (OpenDefinitionAfter ref)
        workspaceItem
        isFocused


viewWorkspaceItems : WorkspaceItems -> List (Html Msg)
viewWorkspaceItems =
    WorkspaceItems.mapToList viewItem
