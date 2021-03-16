module KeyboardShortcuts exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)


type Shortcut
    = Single String
    | Sequence String String
    | Combination String String



-- HELPERS


indexToShortcut : Int -> Maybe String
indexToShortcut index =
    let
        n =
            index + 1
    in
    if n > 9 then
        Nothing

    else
        Just (String.fromInt n)



-- VIEW


viewKey : String -> Html msg
viewKey key =
    span [ class "key" ] [ text key ]


viewShortcut : Shortcut -> Html msg
viewShortcut shortcut =
    let
        instruction text_ =
            span [ class "shortcut-instruction" ] [ text text_ ]

        content =
            case shortcut of
                Single key ->
                    [ viewKey key ]

                Sequence keyA keyB ->
                    [ viewKey keyA
                    , instruction "then"
                    , viewKey keyB
                    ]

                Combination mod key ->
                    [ viewKey mod
                    , instruction "plus"
                    , viewKey key
                    ]
    in
    span [ class "keyboard-shortcut" ] content



-- EVENTS


stopPropagationOnKeyup : (KeyboardEvent -> msg) -> Html.Attribute msg
stopPropagationOnKeyup toMsg =
    Html.Events.custom "keyup"
        (decodeKey toMsg
            |> Decode.andThen
                (\msg ->
                    Decode.succeed
                        { message = msg
                        , stopPropagation = True
                        , preventDefault = False
                        }
                )
        )


stopPropagationOnKeydown : (KeyboardEvent -> msg) -> Html.Attribute msg
stopPropagationOnKeydown toMsg =
    Html.Events.custom "keydown"
        (decodeKey toMsg
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


decodeKey : (KeyboardEvent -> msg) -> Decode.Decoder msg
decodeKey toMsg =
    Decode.map toMsg decodeKeyboardEvent
