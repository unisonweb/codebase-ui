module Code.Namespace.NamespaceRef exposing (NamespaceRef(..), toString)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)


type NamespaceRef
    = HashRef Hash
    | NameRef FQN


toString : NamespaceRef -> String
toString id =
    case id of
        HashRef h ->
            Hash.toString h

        NameRef fqn ->
            FQN.toString fqn
