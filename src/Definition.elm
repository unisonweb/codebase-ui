module Definition exposing (..)

import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (andThen, field)


type alias TermSignature =
    List String


type alias TermBody =
    List String


type alias TypeBody =
    List String


type Definition
    = Term Hash FQN TermSignature TermBody
    | Type Hash FQN TypeBody



-- Helpers


hash : Definition -> Hash
hash definition =
    case definition of
        Term hash_ _ _ _ ->
            hash_

        Type hash_ _ _ ->
            hash_


fqn : Definition -> FQN
fqn definition =
    case definition of
        Term _ fqn_ _ _ ->
            fqn_

        Type _ fqn_ _ ->
            fqn_


unqualifiedName : Definition -> String
unqualifiedName definition =
    definition
        |> fqn
        |> FullyQualifiedName.unqualifiedName



-- JSON Decode


decodeType : Decode.Decoder Definition
decodeType =
    let
        decodeHash =
            Decode.map Hash.fromString (field "prefix" Decode.string)

        decodeFqn =
            Decode.map FullyQualifiedName.fromString (field "prefix" Decode.string)
    in
    Decode.map3 Type
        (Decode.index 0 decodeHash)
        (Decode.index 0 decodeFqn)
        (Decode.succeed [])


decodeTerm : Decode.Decoder Definition
decodeTerm =
    let
        decodeHash =
            Decode.map Hash.fromString (field "prefix" Decode.string)

        decodeFqn =
            Decode.map FullyQualifiedName.fromString (field "prefix" Decode.string)
    in
    Decode.map4 Term
        (Decode.index 0 decodeHash)
        (Decode.index 0 decodeFqn)
        (Decode.succeed [])
        (Decode.succeed [])


decodeList : Decode.Decoder (List Definition)
decodeList =
    Decode.map2 List.append
        (field "termDefinitions" (Decode.list decodeTerm))
        (field "typeDefinitions" (Decode.list decodeType))
