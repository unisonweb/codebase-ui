module Project exposing (..)

import FullyQualifiedName exposing (FQN)
import Json.Decode as Decode


type Owner
    = Owner String


type alias Project a =
    { a | owner : Owner, name : FQN }


type alias ProjectListing =
    Project {}


ownerToString : Owner -> String
ownerToString (Owner o) =
    o


decodeList : Decode.Decoder (List ProjectListing)
decodeList =
    Decode.succeed []
