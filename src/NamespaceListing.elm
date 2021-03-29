module NamespaceListing exposing
    ( DefinitionListing(..)
    , NamespaceListing(..)
    , NamespaceListingContent
    , decode
    , map
    )

import Definition.Category as Category exposing (Category, TermCategory(..), TypeCategory(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (andThen, field)
import Json.Decode.Extra exposing (when)
import RemoteData exposing (RemoteData(..), WebData)


type DefinitionListing
    = TypeListing Hash FQN Category
    | TermListing Hash FQN Category
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
    Decode.map SubNamespace
        (Decode.map3 NamespaceListing
            (field "namespaceHash" Hash.decode)
            (field "namespaceName" (FQN.decodeFromParent parentFqn))
            (Decode.succeed NotAsked)
        )


decodeContent : FQN -> Decode.Decoder NamespaceListingContent
decodeContent parentFqn =
    let
        decodeTag =
            field "tag" Decode.string

        emptyNamespaceContent =
            { definitions = [], namespaces = [] }

        decodeTypeTag =
            Decode.oneOf
                [ when (field "typeTag" Decode.string) ((==) "Data") (Decode.succeed (Category.Type DataType))
                , when (field "typeTag" Decode.string) ((==) "Ability") (Decode.succeed (Category.Type AbilityType))
                ]

        decodeTypeListing =
            Decode.map SubDefinition
                (Decode.map3 TypeListing
                    (field "typeHash" Hash.decode)
                    (field "typeName" (FQN.decodeFromParent parentFqn))
                    decodeTypeTag
                )

        decodeTermTag =
            Decode.oneOf
                [ when (field "termTag" Decode.string) ((==) "Test") (Decode.succeed (Category.Term TestTerm))
                , when (field "termTag" Decode.string) ((==) "Doc") (Decode.succeed (Category.Term DocTerm))
                , Decode.succeed (Category.Term PlainTerm)
                ]

        decodeTermListing =
            Decode.map SubDefinition
                (Decode.map3 TermListing
                    (field "termHash" Hash.decode)
                    (field "termName" (FQN.decodeFromParent parentFqn))
                    decodeTermTag
                )

        decodePatchListing =
            Decode.map SubDefinition (Decode.map PatchListing (field "patchName" Decode.string))

        decodeChild =
            Decode.oneOf
                [ when decodeTag ((==) "Subnamespace") (field "contents" (decodeSubNamespace parentFqn))
                , when decodeTag ((==) "TypeObject") (field "contents" decodeTypeListing)
                , when decodeTag ((==) "TermObject") (field "contents" decodeTermListing)
                , when decodeTag ((==) "PatchObject") (field "contents" decodePatchListing)
                ]

        groupContent namespaceChild acc =
            case namespaceChild of
                SubNamespace namespace ->
                    { acc | namespaces = acc.namespaces ++ [ namespace ] }

                SubDefinition definition ->
                    { acc | definitions = acc.definitions ++ [ definition ] }

        childrenToContent children =
            List.foldl groupContent emptyNamespaceContent children
    in
    Decode.list decodeChild |> andThen (childrenToContent >> Decode.succeed)
