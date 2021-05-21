module Workspace exposing (Model, Msg, OutMsg(..), init, open, subscriptions, update, view)

import Api exposing (ApiBasePath, ApiRequest)
import Browser.Dom as Dom
import Definition.Reference as Reference exposing (Reference)
import Env exposing (Env)
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
import Workspace.Zoom as Zoom exposing (Zoom(..))



-- MODEL


type alias Model =
    WorkspaceItems


init : Env -> Maybe Reference -> ( Model, Cmd Msg )
init env mRef =
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
                    open env model ref
            in
            ( m, c )



-- UPDATE


type Msg
    = NoOp
    | Find
    | OpenDefinitionRelativeTo Reference Reference
    | CloseDefinition Reference
    | UpdateZoom Reference Zoom
    | FetchItemFinished Reference (Result Http.Error Item)
    | Keydown KeyboardEvent


type OutMsg
    = None
    | Focused Reference
    | Emptied
    | ShowFinderRequest


update : Env -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update env msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, None )

        Find ->
            ( model, Cmd.none, ShowFinderRequest )

        OpenDefinitionRelativeTo relativeToRef ref ->
            openItem env.apiBasePath model (Just relativeToRef) ref

        CloseDefinition ref ->
            let
                nextModel =
                    WorkspaceItems.remove model ref
            in
            ( nextModel, Cmd.none, openDefinitionsFocusToOutMsg nextModel )

        UpdateZoom ref zoom ->
            let
                updateMatching workspaceItem =
                    case workspaceItem of
                        Success r i _ ->
                            if ref == r then
                                Success r i zoom

                            else
                                workspaceItem

                        _ ->
                            workspaceItem
            in
            ( WorkspaceItems.map updateMatching model, Cmd.none, None )

        FetchItemFinished ref itemResult ->
            let
                workspaceItem =
                    case itemResult of
                        Err e ->
                            WorkspaceItem.Failure ref e

                        Ok i ->
                            WorkspaceItem.Success ref i Zoom.Medium

                nextWorkspaceItems =
                    WorkspaceItems.replace model ref workspaceItem
            in
            ( nextWorkspaceItems, Cmd.none, openDefinitionsFocusToOutMsg nextWorkspaceItems )

        Keydown event ->
            keydown model event



-- UPDATE HELPERS


open : { a | apiBasePath : ApiBasePath } -> Model -> Reference -> ( Model, Cmd Msg, OutMsg )
open cfg model ref =
    openItem cfg.apiBasePath model Nothing ref


openItem : ApiBasePath -> Model -> Maybe Reference -> Reference -> ( Model, Cmd Msg, OutMsg )
openItem apiBasePath model relativeToRef ref =
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
                case relativeToRef of
                    Nothing ->
                        WorkspaceItems.insertWithFocus model toInsert

                    Just r ->
                        -- WorkspaceItems.insertWithFocusAfter model r toInsert
                        WorkspaceItems.insertWithFocusBefore model r toInsert
        in
        ( nextWorkspaceItems
        , Cmd.batch [ Api.perform apiBasePath (fetchDefinition ref), scrollToDefinition ref ]
        , openDefinitionsFocusToOutMsg nextWorkspaceItems
        )


openDefinitionsFocusToOutMsg : WorkspaceItems -> OutMsg
openDefinitionsFocusToOutMsg openDefs =
    let
        toFocusedOut workspaceItem =
            case workspaceItem of
                Success ref _ _ ->
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
    in
    case keyboardEvent.key of
        ArrowDown ->
            nextDefinition

        J _ ->
            nextDefinition

        ArrowUp ->
            prevDefinitions

        K _ ->
            prevDefinitions

        Space ->
            let
                cycleZoom items ref =
                    let
                        mapper item =
                            case item of
                                Success r i z ->
                                    if r == ref then
                                        Success r i (Zoom.cycle z)

                                    else
                                        item

                                _ ->
                                    item
                    in
                    WorkspaceItems.map mapper items

                cycled =
                    model
                        |> WorkspaceItems.focus
                        |> Maybe.map (WorkspaceItem.reference >> cycleZoom model)
                        |> Maybe.withDefault model
            in
            ( cycled, Cmd.none, None )

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
            ( model, Cmd.none, None )



-- EFFECTS


fetchDefinition : Reference -> ApiRequest Item Msg
fetchDefinition ref =
    Api.getDefinition [ (Reference.hashQualified >> HashQualified.toString) ref ]
        |> Api.toRequest WorkspaceItem.decodeItem (FetchItemFinished ref)


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
        (OpenDefinitionRelativeTo ref)
        (UpdateZoom ref)
        workspaceItem
        isFocused


viewWorkspaceItems : WorkspaceItems -> List (Html Msg)
viewWorkspaceItems =
    WorkspaceItems.mapToList viewItem
