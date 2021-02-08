module Hash exposing (Hash(..), equals, toString)


type Hash
    = Hash String


equals : Hash -> Hash -> Bool
equals (Hash a) (Hash b) =
    a == b


toString : Hash -> String
toString (Hash raw) =
    raw
