module NamespaceListing exposing
    ( DefinitionListing(..)
    , NamespaceListing(..)
    , NamespaceListingContent
    , decode
    , map
    )

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (andThen, field)
import RemoteData exposing (RemoteData(..), WebData)


type DefinitionListing
    = TypeListing Hash FQN
    | TermListing Hash FQN
    | PatchListing String


type alias NamespaceListingContent =
    { definitions : List DefinitionListing, namespaces : List NamespaceListing }


type NamespaceListing
    = NamespaceListing Hash FQN (WebData NamespaceListingContent)


map :
    (NamespaceListing -> NamespaceListing)
    -> NamespaceListing
    -> NamespaceListing
map f (NamespaceListing hash fqn content) =
    let
        mapContent c =
            { c | namespaces = List.map (map f) c.namespaces }
    in
    f (NamespaceListing hash fqn (RemoteData.map mapContent content))



-- JSON DECODE


decode : FQN -> Decode.Decoder NamespaceListing
decode listingFqn =
    Decode.map3
        NamespaceListing
        (field "namespaceListingHash" Hash.decode)
        (field "namespaceListingName" FQN.decode)
        -- The main namespace being decoded has children, so we use Success for
        -- the RemoteData. There children of the children however are not yet
        -- fetched
        (field "namespaceListingChildren" (decodeContent listingFqn)
            |> andThen
                (Success >> Decode.succeed)
        )



-- JSON Decode Helpers


{-| Decoding specific intermediate type |
-}
type DecodedNamespaceChild
    = SubNamespace NamespaceListing
    | SubDefinition DefinitionListing


decodeSubNamespace : FQN -> Decode.Decoder DecodedNamespaceChild
decodeSubNamespace parentFqn =
    Decode.map3 NamespaceListing
        (field "namespaceHash" Hash.decode)
        (field "namespaceName" (FQN.decodeFromParent parentFqn))
        (Decode.succeed NotAsked)
        |> andThen (SubNamespace >> Decode.succeed)


decodeSubDefinition : FQN -> Decode.Decoder DecodedNamespaceChild
decodeSubDefinition parentFqn =
    Decode.oneOf
        [ Decode.map2 TypeListing
            (field "typeHash" Hash.decode)
            (field "typeName" (FQN.decodeFromParent parentFqn))
        , Decode.map2 TermListing
            (field "termHash" Hash.decode)
            (field "termName" (FQN.decodeFromParent parentFqn))
        , Decode.map PatchListing (field "patchName" Decode.string)
        ]
        |> andThen (SubDefinition >> Decode.succeed)


decodeContent : FQN -> Decode.Decoder NamespaceListingContent
decodeContent parentFqn =
    let
        emptyNamespaceContent =
            { definitions = [], namespaces = [] }

        decodeNamespaceChild =
            field "contents"
                (Decode.oneOf [ decodeSubNamespace parentFqn, decodeSubDefinition parentFqn ])

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
