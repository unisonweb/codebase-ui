module Hash exposing (Hash, decode, equals, fromString, toString)

import Json.Decode as Decode


type Hash
    = Hash String


equals : Hash -> Hash -> Bool
equals (Hash a) (Hash b) =
    a == b


toString : Hash -> String
toString (Hash raw) =
    raw


fromString : String -> Hash
fromString raw =
    Hash raw



-- JSON Decode


decode : String -> Decode.Decoder Hash
decode =
    fromString >> Decode.succeed
