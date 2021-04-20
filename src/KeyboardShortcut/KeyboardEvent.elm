module KeyboardShortcut.KeyboardEvent exposing
    ( KeyboardEvent
    , KeyboardEventType(..)
    , considerKeyboardEvent
    , decode
    , isHoldingModifier
    , modifiersHeld
    , stopPropagationOn
    )

import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder, andThen, bool, fail, field, map7, string, succeed)
import KeyboardShortcut.Key as Key exposing (Key(..))
import List.Nonempty as NEL
import Maybe.Extra as MaybeE


type KeyboardEventType
    = Keyup
    | Keydown
    | Keypress



{--| KeyboardEvent

Modelled after https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent

It does **not** work on ASCII codes as those are all deprecated and relaced by
Key and Code. This also means that this module doesn't support older browsers.

* https://caniuse.com/keyboardevent-key
* https://caniuse.com/keyboardevent-code

--}


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


keyboardEventTypeToEventString : KeyboardEventType -> String
keyboardEventTypeToEventString keyboardEventType =
    case keyboardEventType of
        Keypress ->
            "keypress"

        Keyup ->
            "keyup"

        Keydown ->
            "keydown"



-- EVENTS


considerKeyboardEvent : (KeyboardEvent -> Maybe msg) -> Decoder msg
considerKeyboardEvent func =
    andThen
        (\event ->
            case func event of
                Just msg ->
                    succeed msg

                Nothing ->
                    fail "Ignoring keyboard event"
        )
        decode


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
