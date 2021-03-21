module HashQualified exposing (..)

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)


type HashQualified
    = HashOnly Hash
    | HashQualified FQN Hash


type ToStringPrefererence
    = PreferName
    | PreferHash


toString : ToStringPrefererence -> HashQualified -> String
toString preference hq =
    case hq of
        HashOnly hash_ ->
            Hash.toString hash_

        HashQualified fqn hash_ ->
            case preference of
                PreferName ->
                    FQN.toString fqn

                PreferHash ->
                    Hash.toString hash_


name : HashQualified -> Maybe FQN
name hq =
    case hq of
        HashOnly _ ->
            Nothing

        HashQualified fqn _ ->
            Just fqn


hash : HashQualified -> Hash
hash hq =
    case hq of
        HashOnly h ->
            h

        HashQualified _ h ->
            h
