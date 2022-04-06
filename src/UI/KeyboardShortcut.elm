{--| 

  KeyboardShortcut
  ================

  This module revolves around the KeyboardShortcut custom type which can be
  either a `Sequence` or a `Chord`.

  A `Sequence` is for keyboard shortcuts that either need just a single key to
  trigger— in which case the last key in the Sequence is all that's required to
  check— or a multiple key presses (limited to 2) done within a short time
  spand of each other, much like how the <leader> shortcut sequences work in
  the Vim editor. Keystrokes are recorded in KeyboardShortcut.Model.
  The `fromKeyboardEvent` helper creates a `Sequence` (when it doesn't match a
  `Chord`) using the last 2 presses made. If there was only, 1 keypress, it's a
  `Sequence` of 1.

  A `Chord` is for combination keyboard shortcuts like CMD+k where the user
  presses a key while holding a modifier.  `fromKeyboardEvent` creates `Chord`
  over `Sequence` when a modifier is held.

  TEA
  ---

  To use this module, attach `KeyboardShortcut.Model`, `KeyboardShortcut.init`,
  `KeyboardShortcut.Msg`, and `KeyboardShortcut.update` in the parent as often
  seen in TEA composition.  Collect keystrokes from the parents keyboard event
  handlers wity `KeyboardShortcut.collect`

  `KeyboardShortcut.view` render shortcut indicators to provide the user with
  instruction on how a shortcut can be performed. If viewing a `Sequence`
  shortcut, the key that initiates the sequence will be highlighted when
  pressed.

--}


module UI.KeyboardShortcut exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class, classList)
import Lib.OperatingSystem exposing (OperatingSystem)
import List.Nonempty as NEL
import Process
import Task
import UI.KeyboardShortcut.Key as Key exposing (Key)
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)


type
    KeyboardShortcut
    -- Example for x: Sequence Nothing (X Lower)
    -- Example for ; then 3: Sequence (Just Semicolon) Three
    = Sequence (Maybe Key) Key
      -- Chords only support only a single modifier by design;
      -- More modifiers = less ergonomic
      -- Example for Ctrl+k: Chord Ctrl (K Lower)
    | Chord Key Key



-- CREATE


single : Key -> KeyboardShortcut
single key =
    Sequence Nothing key



-- MODEL


type alias Model =
    { key : Maybe Key, operatingSystem : OperatingSystem }


init : OperatingSystem -> Model
init os =
    { key = Nothing, operatingSystem = os }



-- UPDATE


type Msg
    = CollectKey Key
    | Decay Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CollectKey key ->
            ( { model | key = Just key }, decay key )

        Decay key ->
            case model.key of
                Just k ->
                    if k == key then
                        ( { model | key = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( { model | key = Nothing }, Cmd.none )


collect : Model -> Key -> ( Model, Cmd Msg )
collect model key =
    update (CollectKey key) model


decay : Key -> Cmd Msg
decay key =
    Process.sleep 3000 |> Task.perform (always (Decay key))



-- HELPERS


fromKeyboardEvent : Model -> KeyboardEvent -> KeyboardShortcut
fromKeyboardEvent model event =
    if Key.isModifier event.key then
        Sequence model.key event.key

    else
        case KeyboardEvent.modifiersHeld event of
            Just modifiers ->
                Chord (NEL.head modifiers) event.key

            Nothing ->
                Sequence model.key event.key


startedSequenceWith : Model -> Key -> Bool
startedSequenceWith model key =
    Just key == model.key



-- VIEW


viewKeyBase : String -> Bool -> Html msg
viewKeyBase key isActive =
    span [ classList [ ( "key", True ), ( "active", isActive ) ] ] [ text key ]


viewBase : List (Html msg) -> Html msg
viewBase shortcut =
    span [ class "keyboard-shortcut" ] shortcut


viewKey : OperatingSystem -> Key -> Bool -> Html msg
viewKey os key isActive =
    viewKeyBase (Key.view os key) isActive


viewThen : Html msg
viewThen =
    span [ class "then" ] [ text "then" ]


view : Model -> KeyboardShortcut -> Html msg
view model shortcut =
    let
        os =
            model.operatingSystem

        content =
            case shortcut of
                Sequence Nothing key ->
                    [ viewKey os key False ]

                Sequence (Just keyA) keyB ->
                    [ viewKey os keyA (startedSequenceWith model keyA)
                    , viewThen
                    , viewKey os keyB False
                    ]

                Chord mod key ->
                    [ viewKey os mod False
                    , viewKey os key False
                    ]
    in
    viewBase content


viewShortcuts : Model -> List KeyboardShortcut -> Html msg
viewShortcuts model shortcuts =
    let
        or =
            span [ class "separator" ] [ text "or" ]

        instructions =
            shortcuts
                |> List.map (view model)
                |> List.intersperse or
    in
    span [ class "keyboard-shortcuts" ] instructions
