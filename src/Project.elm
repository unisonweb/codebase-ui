module Project exposing (..)

import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode


type Owner
    = Owner String


type alias Project a =
    { a | owner : Owner, name : FQN, hash : Hash }


type alias ProjectListing =
    Project ()


decodeList : Decode.Decoder (List ProjectListing)
decodeList =
    Decode.succeed []
