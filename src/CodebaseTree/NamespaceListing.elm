module CodebaseTree.NamespaceListing exposing
    ( DefinitionListing(..)
    , NamespaceListing(..)
    , NamespaceListingChild(..)
    , NamespaceListingContent
    , contentFetched
    , decode
    , map
    )

import Definition.Category as Category exposing (Category)
import Definition.Term as Term
import Definition.Type as Type
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
    List NamespaceListingChild


type NamespaceListingChild
    = SubNamespace NamespaceListing
    | SubDefinition DefinitionListing


type NamespaceListing
    = NamespaceListing Hash FQN (WebData NamespaceListingContent)


map :
    (NamespaceListing -> NamespaceListing)
    -> NamespaceListing
    -> NamespaceListing
map f (NamespaceListing hash fqn content) =
    let
        mapNamespaceListing c =
            case c of
                SubNamespace nl ->
                    SubNamespace (map f nl)

                _ ->
                    c
    in
    f (NamespaceListing hash fqn (RemoteData.map (List.map mapNamespaceListing) content))


contentFetched : NamespaceListing -> FQN -> Bool
contentFetched (NamespaceListing _ fqn content) needleFqn =
    let
        contentFetched_ child =
            case child of
                SubNamespace nl ->
                    contentFetched nl needleFqn

                _ ->
                    False

        contentIncludes c =
            List.any contentFetched_ c
    in
    (FQN.equals fqn needleFqn && RemoteData.isSuccess content)
        || (content |> RemoteData.map contentIncludes |> RemoteData.withDefault False)



-- JSON DECODE


decode : FQN -> Decode.Decoder NamespaceListing
decode listingFqn =
    Decode.map3
        NamespaceListing
        (field "namespaceListingHash" Hash.decode)
        (field "namespaceListingFQN" FQN.decode)
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
decodeSubNamespace : FQN -> Decode.Decoder NamespaceListingChild
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
            if Hash.isAbilityConstructorHash hash then
                "AbilityConstructor"

            else if Hash.isDataConstructorHash hash then
                "DataConstructor"

            else
                "Term"

        decodeConstructorSuffix =
            Decode.map termTypeByHash (at [ "contents", "termHash" ] Hash.decode)

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
                    (Decode.map Category.Type (Type.decodeTypeCategory [ "typeTag" ]))
                )

        decodeTermListing =
            Decode.map SubDefinition
                (Decode.map3 TermListing
                    (field "termHash" Hash.decode)
                    (field "termName" (FQN.decodeFromParent parentFqn))
                    (Decode.map Category.Term (Term.decodeTermCategory [ "termTag" ]))
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
    in
    Decode.list decodeChild
