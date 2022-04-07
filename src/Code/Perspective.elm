module Code.Perspective exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.Namespace exposing (NamespaceDetails)
import Json.Decode as Decode exposing (field)
import RemoteData exposing (RemoteData(..), WebData)



{-
   Perspective
   ===========

   A Perspective (and PerspectiveParams) is used to orient how Code is viewed
   (in terms of page, url, and api requests) request. Have the user changed
   perspective to view a sub namespace within a project? Perhaps they are
   viewing the root of their codebase, etc. This information is tracked with a
   `Perspective`.

-}


type
    Perspective
    -- The Root can refer to several things; the root of the codebase,
    -- the root of a project, or a users codebase.
    = Root Hash
    | Namespace
        { rootHash : Hash
        , fqn : FQN
        , details : WebData NamespaceDetails
        }


toRootPerspective : Perspective -> Perspective
toRootPerspective perspective =
    Root (rootHash perspective)


toNamespacePerspective : Perspective -> FQN -> Perspective
toNamespacePerspective perspective fqn_ =
    Namespace { rootHash = rootHash perspective, fqn = fqn_, details = NotAsked }


rootHash : Perspective -> Hash
rootHash perspective =
    case perspective of
        Root hash_ ->
            hash_

        Namespace d ->
            d.rootHash


fqn : Perspective -> FQN
fqn perspective =
    case perspective of
        Root _ ->
            FQN.fromString "."

        Namespace d ->
            d.fqn


equals : Perspective -> Perspective -> Bool
equals a b =
    case ( a, b ) of
        ( Root ah, Root bh ) ->
            Hash.equals ah bh

        ( Namespace ans, Namespace bns ) ->
            Hash.equals ans.rootHash bns.rootHash && FQN.equals ans.fqn bns.fqn

        _ ->
            False


{-| Even when we have a Root hash, we always constructor Relative params.
Absolute is currently not supported (until Unison Share includes historic
root), though the model allows it.
-}
toParams : Perspective -> PerspectiveParams
toParams perspective =
    case perspective of
        Root _ ->
            ByRoot Relative

        Namespace d ->
            ByNamespace Relative d.fqn


fromParams : PerspectiveParams -> Maybe Perspective
fromParams params =
    case params of
        ByRoot Relative ->
            Nothing

        ByNamespace Relative _ ->
            Nothing

        ByRoot (Absolute h) ->
            Just (Root h)

        ByNamespace (Absolute h) fqn_ ->
            Just (Namespace { rootHash = h, fqn = fqn_, details = NotAsked })


{-| Similar to `fromParams`, but requires a previous `Perspective` (with a
root hash) to migrate from
-}
nextFromParams : Perspective -> PerspectiveParams -> Perspective
nextFromParams perspective params =
    let
        rootHash_ =
            rootHash perspective
    in
    case ( params, perspective ) of
        ( ByNamespace Relative fqn_, Namespace d ) ->
            if Hash.equals rootHash_ d.rootHash && FQN.equals fqn_ d.fqn then
                Namespace d

            else
                Namespace { rootHash = rootHash_, fqn = fqn_, details = NotAsked }

        ( ByNamespace (Absolute h) fqn_, Namespace d ) ->
            if Hash.equals h d.rootHash && FQN.equals fqn_ d.fqn then
                Namespace d

            else
                Namespace { rootHash = h, fqn = fqn_, details = NotAsked }

        ( ByNamespace Relative fqn_, _ ) ->
            Namespace { rootHash = rootHash_, fqn = fqn_, details = NotAsked }

        ( ByNamespace (Absolute h) fqn_, _ ) ->
            Namespace { rootHash = h, fqn = fqn_, details = NotAsked }

        ( ByRoot Relative, _ ) ->
            Root rootHash_

        ( ByRoot (Absolute h), _ ) ->
            Root h


needsFetching : Perspective -> Bool
needsFetching perspective =
    case perspective of
        Namespace d ->
            d.details == NotAsked

        _ ->
            False


isRootPerspective : Perspective -> Bool
isRootPerspective perspective =
    case perspective of
        Root _ ->
            True

        Namespace _ ->
            False


isNamespacePerspective : Perspective -> Bool
isNamespacePerspective perspective =
    case perspective of
        Root _ ->
            False

        Namespace _ ->
            True



-- Decode ---------------------------------------------------------------------


decode : PerspectiveParams -> Decode.Decoder Perspective
decode perspectiveParams =
    let
        make rootHash_ =
            case perspectiveParams of
                ByRoot _ ->
                    Root rootHash_

                ByNamespace _ fqn_ ->
                    Namespace { rootHash = rootHash_, fqn = fqn_, details = NotAsked }
    in
    Decode.map make (field "namespaceListingHash" Hash.decode)



-- PerspectiveParams ----------------------------------------------------------
-- These are how a perspective is represented in the url, supporting relative
-- URLs; "latest" vs the absolute URL with a root hash.


type RootPerspectiveParam
    = Relative
    | Absolute Hash


type PerspectiveParams
    = ByRoot RootPerspectiveParam
    | ByNamespace RootPerspectiveParam FQN
