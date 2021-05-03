module NamespaceListing exposing
    ( DefinitionListing(..)
    , NamespaceListing(..)
    , NamespaceListingContent
    , contentFetched
    , decode
    , map
    )

import Definition.AbilityConstructor as AbilityConstructor
import Definition.Category as Category exposing (Category)
import Definition.DataConstructor as DataConstructor
import Definition.Term as Term exposing (TermCategory(..))
import Definition.Type as Type exposing (TypeCategory(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (andThen, at, field)
import Json.Decode.Extra exposing (when)
import RemoteData exposing (RemoteData(..), WebData)


type DefinitionListing
    = TypeListing Hash FQN Category
    | TermListing Hash FQN Category
    | DataConstructorListing Hash FQN
    | AbilityConstructorListing Hash FQN
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


contentFetched : NamespaceListing -> FQN -> Bool
contentFetched (NamespaceListing _ fqn content) needleFqn =
    let
        contentIncludes c =
            List.any (\l -> contentFetched l needleFqn) c.namespaces
    in
    (FQN.equals fqn needleFqn && RemoteData.isSuccess content)
        || (content |> RemoteData.map contentIncludes |> RemoteData.withDefault False)



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

        termTypeByHash hash =
            if AbilityConstructor.isAbilityConstructorHash hash then
                "AbilityConstructor"

            else if DataConstructor.isDataConstructorHash hash then
                "DataConstructor"

            else
                "Term"

        decodeConstructorSuffix =
            Decode.map termTypeByHash (at [ "contents", "termHash" ] Hash.decode)

        emptyNamespaceContent =
            { definitions = [], namespaces = [] }

        decodeAbilityConstructorListing =
            Decode.map SubDefinition
                (Decode.map2 AbilityConstructorListing
                    (field "termHash" Hash.decode)
                    (field "termName" (FQN.decodeFromParent parentFqn))
                )

        decodeDataConstructorListing =
            Decode.map SubDefinition
                (Decode.map2 DataConstructorListing
                    (field "termHash" Hash.decode)
                    (field "termName" (FQN.decodeFromParent parentFqn))
                )

        decodeTypeListing =
            Decode.map SubDefinition
                (Decode.map3 TypeListing
                    (field "typeHash" Hash.decode)
                    (field "typeName" (FQN.decodeFromParent parentFqn))
                    (Decode.map Category.Type (Type.decodeTypeCategory "typeTag"))
                )

        decodeTermListing =
            Decode.map SubDefinition
                (Decode.map3 TermListing
                    (field "termHash" Hash.decode)
                    (field "termName" (FQN.decodeFromParent parentFqn))
                    (Decode.map Category.Term (Term.decodeTermCategory "termTag"))
                )

        decodePatchListing =
            Decode.map SubDefinition (Decode.map PatchListing (field "patchName" Decode.string))

        decodeChild =
            Decode.oneOf
                [ when decodeTag ((==) "Subnamespace") (field "contents" (decodeSubNamespace parentFqn))
                , when decodeConstructorSuffix ((==) "DataConstructor") (field "contents" decodeDataConstructorListing)
                , when decodeConstructorSuffix ((==) "AbilityConstructor") (field "contents" decodeAbilityConstructorListing)
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
