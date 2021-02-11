module NamespaceListing exposing
    ( DefinitionListing(..)
    , NamespaceListing(..)
    , NamespaceListingContent
    , decode
    , map
    )

import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (andThen, field)
import List.Nonempty
import RemoteData exposing (RemoteData(..), WebData)


type DefinitionListing
    = Type Hash FQN
    | Term Hash FQN
    | Patch String


type alias NamespaceListingContent =
    { definitions : List DefinitionListing, namespaces : List NamespaceListing }


type NamespaceListing
    = NamespaceListing Hash FQN (WebData NamespaceListingContent)


map : (NamespaceListing -> NamespaceListing) -> NamespaceListing -> NamespaceListing
map f ((NamespaceListing hash fqn content) as namespace) =
    let
        mapContent c =
            { c | namespaces = List.map (map f) c.namespaces }
    in
    f (NamespaceListing hash fqn (RemoteData.map mapContent content))



-- JSON Decode


decode : Decode.Decoder NamespaceListing
decode =
    Decode.map3
        NamespaceListing
        (field "namespaceListingHash" Decode.string |> andThen Hash.decode)
        (field "namespaceListingName" Decode.string |> andThen FullyQualifiedName.decode)
        -- The main namespace being decoded has children, so we use Success for
        -- the RemoteData. There children of the children however are not yet
        -- fetched
        (field "namespaceListingChildren" decodeContent |> andThen (Success >> Decode.succeed))



-- JSON Decode Helpers


{-| Decoding specific intermediate type |
-}
type DecodedNamespaceChild
    = SubNamespace NamespaceListing
    | SubDefinition DefinitionListing


decodeSubNamespace : Decode.Decoder DecodedNamespaceChild
decodeSubNamespace =
    Decode.map3 NamespaceListing
        -- TODO namespaceName should be namespaceHash
        (field "namespaceName" Decode.string |> andThen Hash.decode)
        (field "namespaceName" Decode.string |> andThen FullyQualifiedName.decode)
        (Decode.succeed NotAsked)
        |> andThen (SubNamespace >> Decode.succeed)


decodeSubDefinition : Decode.Decoder DecodedNamespaceChild
decodeSubDefinition =
    Decode.oneOf
        [ Decode.map2 Type
            (field "typeHash" Decode.string |> andThen Hash.decode)
            (field "typeName" Decode.string |> andThen FullyQualifiedName.decode)
        , Decode.map2 Term
            (field "termHash" Decode.string |> andThen Hash.decode)
            (field "termName" Decode.string |> andThen FullyQualifiedName.decode)
        , Decode.map Patch (field "patchName" Decode.string)
        ]
        |> andThen (SubDefinition >> Decode.succeed)


decodeContent : Decode.Decoder NamespaceListingContent
decodeContent =
    let
        emptyNamespaceContent =
            { definitions = [], namespaces = [] }

        decodeNamespaceChild =
            field "contents"
                (Decode.oneOf [ decodeSubNamespace, decodeSubDefinition ])

        groupContent namespaceChild acc =
            case namespaceChild of
                SubNamespace namespace ->
                    { acc | namespaces = acc.namespaces ++ [ namespace ] }

                SubDefinition definition ->
                    { acc | definitions = acc.definitions ++ [ definition ] }

        childrenToContent children =
            List.foldl groupContent emptyNamespaceContent children
    in
    Decode.list decodeNamespaceChild |> andThen (childrenToContent >> Decode.succeed)
