{--| 

  KeyboardShortcut.KeyboardEvent
  ==============================

  Modelled after https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent

  It specifically does **not** work on the ASCII code properties
  (`keyCode`, `charCode`, etc) as those are all deprecated and replaced by
  `key` and `code`.
  This also means that this module doesn't support older browsers.

  * https://caniuse.com/keyboardevent-key
  * https://caniuse.com/keyboardevent-code

  `key` is decoded to `KeyboardShortcut.Key`.

  Event Handlers
  --------------

  It's possible to use the core Elm event handlers with KeyboardEvent.decode,
  but this module also provides a few helpers that directly relates to
  subscribing to keyboard events:

  * `on` and `stopPropagationOn` for DOM node Attribute style keyboard events
  * `subscribe` for global/window keyboard events through Sub

  All of these take the `KeyboardEventType` custom type for configuring which
  kind of event to listen for.

  Modifiers
  ---------

  A few keyboard modifier helper functions are included for detirmining if a
  modifier is being held.

--}


module KeyboardShortcut.KeyboardEvent exposing
    ( KeyboardEvent
    , KeyboardEventType(..)
    , decode
    , decodeToMsg
    , isHoldingModifier
    , modifiersHeld
    , on
    , stopPropagationOn
    , subscribe
    )

import Browser.Events
import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder, bool, field, map7, string)
import KeyboardShortcut.Key as Key exposing (Key(..))
import List.Nonempty as NEL
import Maybe.Extra as MaybeE


type KeyboardEventType
    = Keyup
    | Keydown
    | Keypress


type alias KeyboardEvent =
    { altKey : Bool
    , ctrlKey : Bool
    , metaKey : Bool
    , shiftKey : Bool
    , repeat : Bool

    {--| Representing
    https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key --}
    , key : Key

    {--| Not an ASCII code, but a string like keyA. See
    https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code --}
    , code : String
    }



-- HELPERS


isHoldingModifier : KeyboardEvent -> Bool
isHoldingModifier { altKey, ctrlKey, metaKey, shiftKey } =
    altKey || ctrlKey || metaKey || shiftKey


modifiersHeld : KeyboardEvent -> Maybe (NEL.Nonempty Key)
modifiersHeld { altKey, ctrlKey, metaKey, shiftKey } =
    let
        heldToKey k held =
            if held then
                Just k

            else
                Nothing

        keysHeld =
            [ heldToKey Alt altKey
            , heldToKey Ctrl ctrlKey
            , heldToKey Meta metaKey
            , heldToKey Shift shiftKey
            ]
    in
    keysHeld
        |> MaybeE.values
        |> NEL.fromList



-- EVENTS


stopPropagationOn : KeyboardEventType -> (KeyboardEvent -> msg) -> Html.Attribute msg
stopPropagationOn keyboardEventType toMsg =
    Html.Events.custom (keyboardEventTypeToEventString keyboardEventType)
        (decodeToMsg toMsg
            |> Decode.andThen
                (\msg ->
                    Decode.succeed
                        { message = msg
                        , stopPropagation = True
                        , preventDefault = False
                        }
                )
        )


on : KeyboardEventType -> (KeyboardEvent -> msg) -> Html.Attribute msg
on eventType toMsg =
    Html.Events.on
        (keyboardEventTypeToEventString eventType)
        (Decode.map toMsg decode)


subscribe : KeyboardEventType -> (KeyboardEvent -> msg) -> Sub msg
subscribe eventType toMsg =
    let
        handler =
            Decode.map toMsg decode
    in
    case eventType of
        Keydown ->
            Browser.Events.onKeyDown handler

        Keyup ->
            Browser.Events.onKeyUp handler

        Keypress ->
            Browser.Events.onKeyPress handler



-- DECODE


decode : Decoder KeyboardEvent
decode =
    map7 KeyboardEvent
        (field "altKey" bool)
        (field "ctrlKey" bool)
        (field "metaKey" bool)
        (field "repeat" bool)
        (field "shiftKey" bool)
        (field "key" Key.decode)
        (field "code" string)


decodeToMsg : (KeyboardEvent -> msg) -> Decode.Decoder msg
decodeToMsg toMsg =
    Decode.map toMsg decode



-- INTERNAL


keyboardEventTypeToEventString : KeyboardEventType -> String
keyboardEventTypeToEventString keyboardEventType =
    case keyboardEventType of
        Keypress ->
            "keypress"

        Keyup ->
            "keyup"

        Keydown ->
            "keydown"
