module Perspective exposing (..)

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (field)
import Namespace exposing (NamespaceDetails)
import RemoteData exposing (RemoteData(..), WebData)


type Perspective
    = Codebase Hash
    | Namespace
        { codebaseHash : Hash
        , fqn : FQN
        , details : WebData NamespaceDetails
        }


toCodebasePerspective : Perspective -> Perspective
toCodebasePerspective perspective =
    Codebase (codebaseHash perspective)


toNamespacePerspective : Perspective -> FQN -> Perspective
toNamespacePerspective perspective fqn_ =
    Namespace { codebaseHash = codebaseHash perspective, fqn = fqn_, details = NotAsked }


codebaseHash : Perspective -> Hash
codebaseHash perspective =
    case perspective of
        Codebase hash_ ->
            hash_

        Namespace d ->
            d.codebaseHash


fqn : Perspective -> FQN
fqn perspective =
    case perspective of
        Codebase _ ->
            FQN.fromString "."

        Namespace d ->
            d.fqn


equals : Perspective -> Perspective -> Bool
equals a b =
    case ( a, b ) of
        ( Codebase ah, Codebase bh ) ->
            Hash.equals ah bh

        ( Namespace ans, Namespace bns ) ->
            Hash.equals ans.codebaseHash bns.codebaseHash && FQN.equals ans.fqn bns.fqn

        _ ->
            False


{-| Even when we have a Codebase hash, we always constructor Relative params.
Absolute is currently not supported (until Unison Share includes historic
codebase), though the model allows it.
-}
toParams : Perspective -> PerspectiveParams
toParams perspective =
    case perspective of
        Codebase _ ->
            ByCodebase Relative

        Namespace d ->
            ByNamespace Relative d.fqn


fromParams : PerspectiveParams -> Maybe Perspective
fromParams params =
    case params of
        ByCodebase Relative ->
            Nothing

        ByNamespace Relative _ ->
            Nothing

        ByCodebase (Absolute h) ->
            Just (Codebase h)

        ByNamespace (Absolute h) fqn_ ->
            Just (Namespace { codebaseHash = h, fqn = fqn_, details = NotAsked })


{-| Similar to `fromParams`, but requires a previous `Perspective` (with a
codebase hash) to migrate from
-}
nextFromParams : Perspective -> PerspectiveParams -> Perspective
nextFromParams perspective params =
    let
        codebaseHash_ =
            codebaseHash perspective
    in
    case ( params, perspective ) of
        ( ByNamespace Relative fqn_, Namespace d ) ->
            if Hash.equals codebaseHash_ d.codebaseHash && FQN.equals fqn_ d.fqn then
                Namespace d

            else
                Namespace { codebaseHash = codebaseHash_, fqn = fqn_, details = NotAsked }

        ( ByNamespace (Absolute h) fqn_, Namespace d ) ->
            if Hash.equals h d.codebaseHash && FQN.equals fqn_ d.fqn then
                Namespace d

            else
                Namespace { codebaseHash = h, fqn = fqn_, details = NotAsked }

        ( ByNamespace Relative fqn_, _ ) ->
            Namespace { codebaseHash = codebaseHash_, fqn = fqn_, details = NotAsked }

        ( ByNamespace (Absolute h) fqn_, _ ) ->
            Namespace { codebaseHash = h, fqn = fqn_, details = NotAsked }

        ( ByCodebase Relative, _ ) ->
            Codebase codebaseHash_

        ( ByCodebase (Absolute h), _ ) ->
            Codebase h


needsFetching : Perspective -> Bool
needsFetching perspective =
    case perspective of
        Namespace d ->
            d.details == NotAsked

        _ ->
            False


isCodebasePerspective : Perspective -> Bool
isCodebasePerspective perspective =
    case perspective of
        Codebase _ ->
            True

        Namespace _ ->
            False


isNamespacePerspective : Perspective -> Bool
isNamespacePerspective perspective =
    case perspective of
        Codebase _ ->
            False

        Namespace _ ->
            True



-- Decode ---------------------------------------------------------------------


decode : PerspectiveParams -> Decode.Decoder Perspective
decode perspectiveParams =
    let
        make codebaseHash_ =
            case perspectiveParams of
                ByCodebase _ ->
                    Codebase codebaseHash_

                ByNamespace _ fqn_ ->
                    Namespace { codebaseHash = codebaseHash_, fqn = fqn_, details = NotAsked }
    in
    Decode.map make (field "namespaceListingHash" Hash.decode)



-- PerspectiveParams ----------------------------------------------------------
-- These are how a perspective is represented in the url, supporting relative
-- URLs; "latest" vs the absolute URL with a codebase hash.


type CodebasePerspectiveParam
    = Relative
    | Absolute Hash


type PerspectiveParams
    = ByCodebase CodebasePerspectiveParam
    | ByNamespace CodebasePerspectiveParam FQN
