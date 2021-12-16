module Project exposing (..)

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (field, string)


type Owner
    = Owner String


type alias Project a =
    { a | owner : Owner, name : FQN, hash : Hash }


type alias ProjectListing =
    Project {}


slug : Project a -> FQN
slug project =
    FQN.cons (ownerToString project.owner) project.name


ownerToString : Owner -> String
ownerToString (Owner o) =
    o



-- Decode


decodeListing : Decode.Decoder ProjectListing
decodeListing =
    let
        mk owner name hash =
            { owner = owner, name = name, hash = hash }
    in
    Decode.map3
        mk
        (field "owner" (Decode.map Owner string))
        (field "name" FQN.decode)
        (field "hash" Hash.decode)


decodeListings : Decode.Decoder (List ProjectListing)
decodeListings =
    Decode.list decodeListing
