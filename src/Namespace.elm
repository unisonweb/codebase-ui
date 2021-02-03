module Namespace exposing (..)

import Definition exposing (Definition)
import Json.Decode as Json exposing (andThen, field)
import List.Nonempty
import UnisonHash exposing (UnisonHash(..))


type Path
    = Path (List.Nonempty.Nonempty String)


type NamespaceChild
    = SubNamespace String Int
    | Type String UnisonHash


type alias Namespace =
    { path : Path, hash : UnisonHash, children : List NamespaceChild }


pathFromString : String -> Path
pathFromString rawPath =
    let
        parts =
            rawPath
                |> String.split "."
                |> List.map String.trim
                |> List.filter (\s -> String.length s > 0)
                |> List.Nonempty.fromList
    in
    case parts of
        Nothing ->
            Path (List.Nonempty.fromElement ".")

        Just path ->
            Path path


name : Path -> String
name (Path path) =
    List.Nonempty.last path



-- JSON Decoders


decodePath : String -> Json.Decoder Path
decodePath rawPath =
    rawPath |> pathFromString |> Json.succeed


decodeNamespaceChildSubNamespace : Json.Decoder NamespaceChild
decodeNamespaceChildSubNamespace =
    Json.map2 SubNamespace
        (field "namespaceName" Json.string)
        (field "namespaceSize" Json.int)


decodeNamespaceChildType : Json.Decoder NamespaceChild
decodeNamespaceChildType =
    Json.map2 Type
        (field "typeName" Json.string)
        (field "typeHash" Json.string |> andThen (UnisonHash >> Json.succeed))


decodeNamespaceChild : Json.Decoder NamespaceChild
decodeNamespaceChild =
    field "contents"
        (Json.oneOf [ decodeNamespaceChildSubNamespace, decodeNamespaceChildType ])


decodeChildren : Json.Decoder (List NamespaceChild)
decodeChildren =
    Json.list decodeNamespaceChild


decode : Json.Decoder Namespace
decode =
    Json.map3
        Namespace
        (field "namespaceListingName" Json.string |> andThen decodePath)
        (field "namespaceListingHash" Json.string |> andThen (UnisonHash >> Json.succeed))
        (field "namespaceListingChildren" decodeChildren)
