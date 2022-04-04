module Lib.Util exposing (..)

import Json.Decode as Decode
import List.Nonempty as NEL
import Process
import Task



-- Various utility functions and helpers


delayMsg : Float -> msg -> Cmd msg
delayMsg delay msg =
    Task.perform (\_ -> msg) (Process.sleep delay)


decodeNonEmptyList : Decode.Decoder a -> Decode.Decoder (NEL.Nonempty a)
decodeNonEmptyList =
    Decode.list
        >> Decode.andThen
            (\list ->
                case NEL.fromList list of
                    Just nel ->
                        Decode.succeed nel

                    Nothing ->
                        Decode.fail "Decoded an empty list"
            )


decodeFailInvalid : String -> Maybe a -> Decode.Decoder a
decodeFailInvalid failMessage m =
    case m of
        Nothing ->
            Decode.fail failMessage

        Just a ->
            Decode.succeed a


decodeTag : Decode.Decoder String
decodeTag =
    Decode.field "tag" Decode.string
