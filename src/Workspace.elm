module Workspace exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , open
    , replaceWorkspaceItemReferencesWithHashOnly
    , subscriptions
    , update
    , view
    )

import Api exposing (ApiRequest)
import Browser.Dom as Dom
import Definition.Doc as Doc
import Definition.Reference as Reference exposing (Reference)
import Env exposing (Env)
import FullyQualifiedName exposing (FQN)
import HashQualified as HQ
import Html exposing (Html, article, div, header, section)
import Html.Attributes exposing (class, id)
import Http
import KeyboardShortcut exposing (KeyboardShortcut(..))
import KeyboardShortcut.Key exposing (Key(..))
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import Perspective exposing (Perspective)
import Task
import UI.Button as Button
import UI.Icon as Icon
import Workspace.WorkspaceItem as WorkspaceItem exposing (Item, WorkspaceItem)
import Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems)



-- MODEL


type alias Model =
    { workspaceItems : WorkspaceItems
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : Env -> Maybe Reference -> ( Model, Cmd Msg )
init env mRef =
    let
        model =
            { workspaceItems = WorkspaceItems.init Nothing
            , keyboardShortcut = KeyboardShortcut.init env.operatingSystem
            }
    in
    case mRef of
        Nothing ->
            ( model, Cmd.none )

        Just ref ->
            let
                ( m, c ) =
                    open env model ref
            in
            ( m, c )



-- UPDATE


type Msg
    = NoOp
    | Find
    | FetchItemFinished Reference (Result Http.Error Item)
    | IsDocCropped Reference (Result Dom.Error Bool)
    | Keydown KeyboardEvent
    | KeyboardShortcutMsg KeyboardShortcut.Msg
    | WorkspaceItemMsg WorkspaceItem.Msg


type OutMsg
    = None
    | Focused Reference
    | Emptied
    | ShowFinderRequest (Maybe FQN)
    | ChangePerspectiveToNamespace FQN


update : Env -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update env msg ({ workspaceItems } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none, None )

        Find ->
            ( model, Cmd.none, ShowFinderRequest Nothing )

        FetchItemFinished ref itemResult ->
            let
                ( workspaceItem, cmd ) =
                    case itemResult of
                        Err e ->
                            ( WorkspaceItem.Failure ref e, Cmd.none )

                        Ok i ->
                            let
                                c =
                                    -- Docs items are always shown in full and never cropped
                                    if WorkspaceItem.isDocItem i then
                                        Cmd.none

                                    else
                                        isDocCropped ref
                            in
                            ( WorkspaceItem.fromItem ref i, c )

                nextWorkspaceItems =
                    WorkspaceItems.replace workspaceItems ref workspaceItem
            in
            ( { model | workspaceItems = nextWorkspaceItems }
            , cmd
            , None
            )

        IsDocCropped ref res ->
            let
                visibility =
                    case res of
                        Ok True ->
                            WorkspaceItem.Cropped

                        Ok False ->
                            WorkspaceItem.NotCropped

                        -- If we can't tell, better make it fully visible, than Unknown
                        Err _ ->
                            WorkspaceItem.MadeFullyVisible

                updateVisibility d =
                    { d | docVisibility = visibility }
            in
            ( { model | workspaceItems = WorkspaceItems.updateData updateVisibility ref workspaceItems }
            , Cmd.none
            , None
            )

        Keydown event ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect model.keyboardShortcut event.key

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event

                ( nextModel, cmd, out ) =
                    handleKeyboardShortcut { model | keyboardShortcut = keyboardShortcut } shortcut
            in
            ( nextModel, Cmd.batch [ cmd, Cmd.map KeyboardShortcutMsg kCmd ], out )

        WorkspaceItemMsg wiMsg ->
            case wiMsg of
                WorkspaceItem.OpenReference relativeToRef ref ->
                    openReference env model relativeToRef ref

                WorkspaceItem.Close ref ->
                    let
                        nextModel =
                            { model | workspaceItems = WorkspaceItems.remove workspaceItems ref }
                    in
                    ( nextModel, Cmd.none, openDefinitionsFocusToOutMsg nextModel.workspaceItems )

                WorkspaceItem.UpdateZoom ref zoom ->
                    let
                        updateZoom d =
                            { d | zoom = zoom }
                    in
                    ( { model | workspaceItems = WorkspaceItems.updateData updateZoom ref workspaceItems }
                    , Cmd.none
                    , None
                    )

                WorkspaceItem.ShowFullDoc ref ->
                    let
                        updateDocVisibility d =
                            { d | docVisibility = WorkspaceItem.MadeFullyVisible }
                    in
                    ( { model | workspaceItems = WorkspaceItems.updateData updateDocVisibility ref workspaceItems }
                    , Cmd.none
                    , None
                    )

                WorkspaceItem.ToggleDocFold ref docId ->
                    let
                        updateDocFoldToggles d =
                            { d | docFoldToggles = Doc.toggleFold d.docFoldToggles docId }
                    in
                    ( { model | workspaceItems = WorkspaceItems.updateData updateDocFoldToggles ref workspaceItems }
                    , Cmd.none
                    , None
                    )

                WorkspaceItem.ChangePerspectiveToNamespace fqn ->
                    ( model, Cmd.none, ChangePerspectiveToNamespace fqn )

                WorkspaceItem.FindWithinNamespace fqn ->
                    ( model, Cmd.none, ShowFinderRequest (Just fqn) )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd, None )



-- UPDATE HELPERS


type alias WithWorkspaceItems m =
    { m | workspaceItems : WorkspaceItems }


replaceWorkspaceItemReferencesWithHashOnly : Model -> Model
replaceWorkspaceItemReferencesWithHashOnly model =
    let
        workspaceItems =
            WorkspaceItems.map WorkspaceItem.toHashReference model.workspaceItems
    in
    { model | workspaceItems = workspaceItems }


open : Env -> WithWorkspaceItems m -> Reference -> ( WithWorkspaceItems m, Cmd Msg )
open env model ref =
    openItem env model Nothing ref


{-| openReference opens a definition relative to another definition. This is
done within Workspace, as opposed to from the outside via a URL change. This
function returns a Focused command for the newly opened reference and as such
changes the URL.
-}
openReference : Env -> WithWorkspaceItems m -> Reference -> Reference -> ( WithWorkspaceItems m, Cmd Msg, OutMsg )
openReference env model relativeToRef ref =
    let
        ( newModel, cmd ) =
            openItem env model (Just relativeToRef) ref

        out =
            openDefinitionsFocusToOutMsg newModel.workspaceItems
    in
    ( newModel, cmd, out )


openItem : Env -> WithWorkspaceItems m -> Maybe Reference -> Reference -> ( WithWorkspaceItems m, Cmd Msg )
openItem env ({ workspaceItems } as model) relativeToRef ref =
    -- We don't want to refetch or replace any already open definitions, but we
    -- do want to focus and scroll to it
    if WorkspaceItems.member workspaceItems ref then
        let
            nextWorkspaceItems =
                WorkspaceItems.focusOn workspaceItems ref
        in
        ( { model | workspaceItems = nextWorkspaceItems }
        , scrollToDefinition ref
        )

    else
        let
            toInsert =
                WorkspaceItem.Loading ref

            nextWorkspaceItems =
                case relativeToRef of
                    Nothing ->
                        WorkspaceItems.prependWithFocus workspaceItems toInsert

                    Just r ->
                        WorkspaceItems.insertWithFocusBefore workspaceItems r toInsert
        in
        ( { model | workspaceItems = nextWorkspaceItems }
        , Cmd.batch [ Api.perform env.apiBasePath (fetchDefinition env.perspective ref), scrollToDefinition ref ]
        )


openDefinitionsFocusToOutMsg : WorkspaceItems -> OutMsg
openDefinitionsFocusToOutMsg openDefs =
    openDefs
        |> WorkspaceItems.focusedReference
        |> Maybe.map Focused
        |> Maybe.withDefault Emptied


handleKeyboardShortcut : Model -> KeyboardShortcut -> ( Model, Cmd Msg, OutMsg )
handleKeyboardShortcut ({ workspaceItems } as model) shortcut =
    let
        scrollToCmd =
            WorkspaceItems.focus
                >> Maybe.map WorkspaceItem.reference
                >> Maybe.map scrollToDefinition
                >> Maybe.withDefault Cmd.none

        nextDefinition =
            let
                next =
                    WorkspaceItems.next model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next, openDefinitionsFocusToOutMsg next )

        prevDefinitions =
            let
                prev =
                    WorkspaceItems.prev model.workspaceItems
            in
            ( { model | workspaceItems = prev }, scrollToCmd prev, openDefinitionsFocusToOutMsg prev )

        moveDown =
            let
                next =
                    WorkspaceItems.moveDown model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next, openDefinitionsFocusToOutMsg next )

        moveUp =
            let
                next =
                    WorkspaceItems.moveUp model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next, openDefinitionsFocusToOutMsg next )
    in
    case shortcut of
        Chord Alt ArrowDown ->
            moveDown

        Chord Alt ArrowUp ->
            moveUp

        {- TODO: Support vim keys for moving. The reason this isn't straight
           forward is that Alt+j results in the "∆" character instead of a "j"
           (k is "˚") on a Mac. We could add those characters as Chord Alt (Raw
           "∆"), but is it uniform that Alt+j produces "∆" across all standard
           international keyboard layouts? KeyboardEvent.code could be used
           instead of KeyboardEvent.key as it will produce the physical key
           pressed as opposed to the key produced —  this of course is strange
           for things like question marks...

              Chord Alt (J _) ->
                  moveDown
              Chord Alt (K _) ->
                  moveUp
        -}
        Sequence _ ArrowDown ->
            nextDefinition

        Sequence _ (J _) ->
            nextDefinition

        Sequence _ ArrowUp ->
            prevDefinitions

        Sequence _ (K _) ->
            prevDefinitions

        Sequence _ Space ->
            let
                cycleZoom wItems ref =
                    WorkspaceItems.updateData WorkspaceItem.cycleZoom ref wItems

                cycled =
                    model.workspaceItems
                        |> WorkspaceItems.focus
                        |> Maybe.map (WorkspaceItem.reference >> cycleZoom model.workspaceItems)
                        |> Maybe.withDefault model.workspaceItems
            in
            ( { model | workspaceItems = cycled }, Cmd.none, None )

        Sequence _ (X _) ->
            let
                without =
                    workspaceItems
                        |> WorkspaceItems.focus
                        |> Maybe.map (WorkspaceItem.reference >> WorkspaceItems.remove workspaceItems)
                        |> Maybe.withDefault workspaceItems
            in
            ( { model | workspaceItems = without }
            , Cmd.none
            , openDefinitionsFocusToOutMsg without
            )

        _ ->
            ( model, Cmd.none, None )



-- EFFECTS


fetchDefinition : Perspective -> Reference -> ApiRequest Item Msg
fetchDefinition perspective ref =
    let
        definitionHash =
            (Reference.hashQualified >> HQ.toString) ref
    in
    [ definitionHash ]
        |> Api.getDefinition perspective
        |> Api.toRequest (WorkspaceItem.decodeItem ref) (FetchItemFinished ref)


isDocCropped : Reference -> Cmd Msg
isDocCropped ref =
    let
        id =
            "definition-doc-" ++ Reference.toString ref
    in
    Dom.getViewportOf id
        |> Task.map (\v -> v.viewport.height < v.scene.height)
        |> Task.attempt (IsDocCropped ref)


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
    case model.workspaceItems of
        WorkspaceItems.Empty ->
            -- TODO: Remove WorkspaceItems.Empty
            -- this state is now determined via Route
            div [] []

        WorkspaceItems.WorkspaceItems _ ->
            article [ id "workspace" ]
                [ header
                    [ id "workspace-toolbar" ]
                    [ Button.iconThenLabel Find Icon.search "Find Definition" |> Button.small |> Button.view
                    ]
                , section
                    [ id "workspace-content" ]
                    [ section [ class "definitions-pane" ] (viewWorkspaceItems model.workspaceItems) ]
                ]


viewItem : WorkspaceItem -> Bool -> Html Msg
viewItem workspaceItem isFocused =
    Html.map WorkspaceItemMsg (WorkspaceItem.view workspaceItem isFocused)


viewWorkspaceItems : WorkspaceItems -> List (Html Msg)
viewWorkspaceItems =
    WorkspaceItems.mapToList viewItem
