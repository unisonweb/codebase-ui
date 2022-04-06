module Code.EntityId exposing (EntityId(..), toString)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)


type EntityId
    = HashId Hash
    | NameId FQN


toString : EntityId -> String
toString id =
    case id of
        HashId h ->
            Hash.toString h

        NameId fqn ->
            FQN.toString fqn
