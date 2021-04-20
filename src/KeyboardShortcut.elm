module KeyboardShortcut exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import KeyboardShortcut.Key as Key exposing (Key)
import KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import List.Nonempty as NEL
import Process
import Task


type KeyboardShortcut
    = Single Key
    | Sequence Key Key
      -- Chords only support only a single modifier by design;
      -- More modifiers = less ergonomic
    | Chord Key Key



-- MODEL


{-| For simple shortcuts, hooking up the model, update and event isn't needed,
but required for Sequence shortcuts as the sequence is tracked in the model
-}
type alias Model =
    Maybe Key


init : Model
init =
    Nothing



-- UPDATE


type Msg
    = CollectKey Key
    | Decay Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CollectKey key ->
            ( Just key, decay key )

        Decay key ->
            case model of
                Just k ->
                    if k == key then
                        ( Nothing, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( Nothing, Cmd.none )


decay : Key -> Cmd Msg
decay key =
    Process.sleep 500 |> Task.perform (always (Decay key))



-- HELPERS


keyboardEventToShortcut : Model -> KeyboardEvent -> KeyboardShortcut
keyboardEventToShortcut model event =
    case KeyboardEvent.modifiersHeld event of
        Just modifiers ->
            Chord (NEL.head modifiers) event.key

        Nothing ->
            case model of
                Just k ->
                    Sequence k event.key

                Nothing ->
                    Single event.key



-- VIEW


viewKey : Key -> Html msg
viewKey key =
    span [ class "key" ] [ text (Key.view key) ]


viewShortcut : KeyboardShortcut -> Html msg
viewShortcut shortcut =
    let
        instruction text_ =
            span [ class "instruction" ] [ text text_ ]

        content =
            case shortcut of
                Single key ->
                    [ viewKey key ]

                Sequence keyA keyB ->
                    [ viewKey keyA
                    , instruction "then"
                    , viewKey keyB
                    ]

                Chord mod key ->
                    [ viewKey mod
                    , instruction "plus"
                    , viewKey key
                    ]
    in
    span [ class "keyboard-shortcut" ] content
