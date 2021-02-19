module FullyQualifiedNameSet exposing (FQNSet, empty, insert, member, remove, toList, toggle)

import FullyQualifiedName as FQN exposing (FQN)
import Set exposing (Set)
import Set.Extra



-- FQNSet - A Set wrapper for FQN


type FQNSet
    = FQNSet (Set String)


size : FQNSet -> Int
size (FQNSet set) =
    Set.size set


empty : FQNSet
empty =
    FQNSet Set.empty


insert : FQN -> FQNSet -> FQNSet
insert fqn (FQNSet set) =
    FQNSet (Set.insert (FQN.toString fqn) set)


remove : FQN -> FQNSet -> FQNSet
remove fqn (FQNSet set) =
    FQNSet (Set.remove (FQN.toString fqn) set)


member : FQN -> FQNSet -> Bool
member fqn (FQNSet set) =
    Set.member (FQN.toString fqn) set


toList : FQNSet -> List FQN
toList (FQNSet set) =
    set
        |> Set.toList
        |> List.map FQN.fromString


toggle : FQN -> FQNSet -> FQNSet
toggle fqn (FQNSet set) =
    FQNSet (Set.Extra.toggle (FQN.toString fqn) set)
