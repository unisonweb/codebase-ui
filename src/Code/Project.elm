module Code.Project exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.Hashvatar as Hashvatar
import Html exposing (Html)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (field, string)
import UI.Click as Click


type Owner
    = Owner String


type alias Project a =
    { a | owner : Owner, name : FQN, hash : Hash }


type alias ProjectListing =
    Project {}


slug : Project a -> FQN
slug project =
    FQN.cons (ownerToString project.owner) project.name


slugString : Project a -> String
slugString project =
    project |> slug |> FQN.toString


ownerToString : Owner -> String
ownerToString (Owner o) =
    o


equals : Project a -> Project b -> Bool
equals a b =
    Hash.equals a.hash b.hash



-- View


viewSlug : Project a -> Html msg
viewSlug project =
    project |> slug |> FQN.view


viewProjectListing : Click.Click msg -> Project a -> Html msg
viewProjectListing click project =
    Click.view [ class "project-listing" ]
        [ Hashvatar.view project.hash
        , viewSlug project
        ]
        click



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
