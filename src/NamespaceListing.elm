module NamespaceListing exposing
    ( DefinitionListing(..)
    , NamespaceListing(..)
    , NamespaceListingContent
    , decode
    , map
    )

import FullyQualifiedName exposing (FQN, fqn)
import Hash exposing (Hash(..))
import Json.Decode as Json exposing (andThen, field)
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


decode : Json.Decoder NamespaceListing
decode =
    Json.map3
        NamespaceListing
        (field "namespaceListingHash" Json.string |> andThen (Hash >> Json.succeed))
        (field "namespaceListingName" Json.string |> andThen (fqn >> Json.succeed))
        -- The main namespace being decoded has children, so we use Success for
        -- the RemoteData. There children of the children however are not yet
        -- fetched
        (field "namespaceListingChildren" decodeContent |> andThen (Success >> Json.succeed))



-- JSON Decode Helpers


{-| Decoding specific intermediate type |
-}
type DecodedNamespaceChild
    = SubNamespace NamespaceListing
    | SubDefinition DefinitionListing


decodeSubNamespace : Json.Decoder DecodedNamespaceChild
decodeSubNamespace =
    Json.map3 NamespaceListing
        -- TODO namespaceName should be namespaceHash
        (field "namespaceName" Json.string |> andThen (Hash >> Json.succeed))
        (field "namespaceName" Json.string |> andThen (fqn >> Json.succeed))
        (Json.succeed NotAsked)
        |> andThen (SubNamespace >> Json.succeed)


decodeSubDefinition : Json.Decoder DecodedNamespaceChild
decodeSubDefinition =
    Json.oneOf
        [ Json.map2 Type
            (field "typeHash" Json.string |> andThen (Hash >> Json.succeed))
            (field "typeName" Json.string |> andThen (fqn >> Json.succeed))
        , Json.map2 Term
            (field "termHash" Json.string |> andThen (Hash >> Json.succeed))
            (field "termName" Json.string |> andThen (fqn >> Json.succeed))
        , Json.map Patch (field "patchName" Json.string)
        ]
        |> andThen (SubDefinition >> Json.succeed)


decodeContent : Json.Decoder NamespaceListingContent
decodeContent =
    let
        emptyNamespaceContent =
            { definitions = [], namespaces = [] }

        decodeNamespaceChild =
            field "contents"
                (Json.oneOf [ decodeSubNamespace, decodeSubDefinition ])

        groupContent namespaceChild acc =
            case namespaceChild of
                SubNamespace namespace ->
                    { acc | namespaces = acc.namespaces ++ [ namespace ] }

                SubDefinition definition ->
                    { acc | definitions = acc.definitions ++ [ definition ] }

        childrenToContent children =
            List.foldl groupContent emptyNamespaceContent children
    in
    Json.list decodeNamespaceChild |> andThen (childrenToContent >> Json.succeed)
