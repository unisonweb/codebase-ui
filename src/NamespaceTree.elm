module NamespaceTree exposing (..)

import FullyQualifiedName exposing (FQN, fqn)
import Hash exposing (Hash(..))
import Json.Decode as Json exposing (andThen, field)
import List.Nonempty
import RemoteData exposing (RemoteData(..), WebData)


type NamespaceTree
    = Namespace Hash FQN (WebData (List NamespaceTree))
    | Type Hash FQN
    | Term Hash FQN
    | Patch String



-- JSON Decoders


decodeSubNamespace : Json.Decoder NamespaceTree
decodeSubNamespace =
    Json.map3 Namespace
        -- TODO namespaceName should be namespaceHash
        (field "namespaceName" Json.string |> andThen (Hash >> Json.succeed))
        (field "namespaceName" Json.string |> andThen (fqn >> Json.succeed))
        (Json.succeed NotAsked)


decodeType : Json.Decoder NamespaceTree
decodeType =
    Json.map2 Type
        (field "typeHash" Json.string |> andThen (Hash >> Json.succeed))
        (field "typeName" Json.string |> andThen (fqn >> Json.succeed))


decodeTerm : Json.Decoder NamespaceTree
decodeTerm =
    Json.map2 Term
        (field "termHash" Json.string |> andThen (Hash >> Json.succeed))
        (field "termName" Json.string |> andThen (fqn >> Json.succeed))


decodePatch : Json.Decoder NamespaceTree
decodePatch =
    Json.map Patch (field "patchName" Json.string)


decodeNamespaceChild : Json.Decoder NamespaceTree
decodeNamespaceChild =
    field "contents"
        (Json.oneOf [ decodeSubNamespace, decodeType, decodePatch ])


decodeChildren : Json.Decoder (List NamespaceTree)
decodeChildren =
    Json.list decodeNamespaceChild


decode : Json.Decoder NamespaceTree
decode =
    Json.map3
        Namespace
        (field "namespaceListingHash" Json.string |> andThen (Hash >> Json.succeed))
        (field "namespaceListingName" Json.string |> andThen (fqn >> Json.succeed))
        (field "namespaceListingChildren" decodeChildren |> andThen (Success >> Json.succeed))
