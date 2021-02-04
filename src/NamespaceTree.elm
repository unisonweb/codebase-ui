module NamespaceTree exposing (..)

import Json.Decode as Json exposing (andThen, field)
import List.Nonempty
import RemoteData exposing (RemoteData(..), WebData)


type NamespaceTree
    = Namespace String (WebData (List NamespaceTree))
    | Type String
    | Term String



-- JSON Decoders


decodeSubNamespace : Json.Decoder NamespaceTree
decodeSubNamespace =
    Json.map2 Namespace
        (field "namespaceName" Json.string)
        (Json.succeed NotAsked)


decodeType : Json.Decoder NamespaceTree
decodeType =
    Json.map Type (field "typeName" Json.string)


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
        (field "namespaceListingName" Json.string)
        (field "namespaceListingChildren" decodeChildren |> andThen (Success >> Json.succeed))
