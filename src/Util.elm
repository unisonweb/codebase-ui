module Util exposing (..)

import Html
import Json.Decode as Decode
import List.Nonempty as NEL



-- Various utility functions and helpers


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
