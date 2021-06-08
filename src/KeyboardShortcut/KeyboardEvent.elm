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
    , attach
    , decode
    , decodeToMsg
    , isHoldingModifier
    , modifiersHeld
    , on
    , preventDefault
    , preventDefaultWhen
    , stopPropagation
    , stopPropagationWhen
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


type alias KeyboardListener msg =
    { keyboardEventType : KeyboardEventType
    , toMsg : KeyboardEvent -> msg
    , stopPropagation : KeyboardEvent -> Bool
    , preventDefault : KeyboardEvent -> Bool
    }


on : KeyboardEventType -> (KeyboardEvent -> msg) -> KeyboardListener msg
on keyboardEventType toMsg =
    { keyboardEventType = keyboardEventType
    , toMsg = toMsg
    , stopPropagation = always False
    , preventDefault = always False
    }


stopPropagation : KeyboardListener msg -> KeyboardListener msg
stopPropagation listener =
    { listener | stopPropagation = always True }


stopPropagationWhen : (KeyboardEvent -> Bool) -> KeyboardListener msg -> KeyboardListener msg
stopPropagationWhen when listener =
    { listener | stopPropagation = when }


preventDefault : KeyboardListener msg -> KeyboardListener msg
preventDefault listener =
    { listener | preventDefault = always True }


preventDefaultWhen : (KeyboardEvent -> Bool) -> KeyboardListener msg -> KeyboardListener msg
preventDefaultWhen when listener =
    { listener | preventDefault = when }


attach : KeyboardListener msg -> Html.Attribute msg
attach listener =
    let
        toEventConfig event =
            { message = listener.toMsg event
            , stopPropagation = listener.stopPropagation event
            , preventDefault = listener.preventDefault event
            }
    in
    Html.Events.custom (keyboardEventTypeToEventString listener.keyboardEventType)
        (Decode.map toEventConfig decode)



{--| Note that there's a limitation to Browser.Event in that it does not support
stopPropagation and preventDefault: https://github.com/elm/browser/issues/77,
https://github.com/elm/browser/issues/89 --}


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
        (field "shiftKey" bool)
        (field "repeat" bool)
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
