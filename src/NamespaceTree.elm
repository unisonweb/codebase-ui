module NamespaceTree exposing (..)

import FullyQualifiedName exposing (FQN, fqn)
import Json.Decode as Json exposing (andThen, field)
import List.Nonempty
import RemoteData exposing (RemoteData(..), WebData)


type NamespaceTree
    = Namespace FQN (WebData (List NamespaceTree))
    | Type FQN
    | Term FQN



-- JSON Decoders


decodeSubNamespace : Json.Decoder NamespaceTree
decodeSubNamespace =
    Json.map2 Namespace
        (field "namespaceName" Json.string |> andThen (fqn >> Json.succeed))
        (Json.succeed NotAsked)


decodeType : Json.Decoder NamespaceTree
decodeType =
    Json.map Type (field "typeName" Json.string |> andThen (fqn >> Json.succeed))


decodeNamespaceChild : Json.Decoder NamespaceTree
decodeNamespaceChild =
    field "contents"
        (Json.oneOf [ decodeSubNamespace, decodeType ])


decodeChildren : Json.Decoder (List NamespaceTree)
decodeChildren =
    Json.list decodeNamespaceChild


decode : Json.Decoder NamespaceTree
decode =
    Json.map2
        Namespace
        (field "namespaceListingName" Json.string |> andThen (fqn >> Json.succeed))
        (field "namespaceListingChildren" decodeChildren |> andThen (Success >> Json.succeed))
