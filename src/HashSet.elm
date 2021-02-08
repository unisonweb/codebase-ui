module HashSet exposing (HashSet, empty, insert, member, remove, toList, toggle)

import Hash exposing (Hash(..))
import Set exposing (Set)
import Set.Extra



-- HashSet - A Set wrapper for Hash


type HashSet
    = HashSet (Set String)


size : HashSet -> Int
size (HashSet set) =
    Set.size set


empty : HashSet
empty =
    HashSet Set.empty


insert : Hash -> HashSet -> HashSet
insert (Hash raw) (HashSet set) =
    HashSet (Set.insert raw set)


remove : Hash -> HashSet -> HashSet
remove (Hash raw) (HashSet set) =
    HashSet (Set.remove raw set)


member : Hash -> HashSet -> Bool
member (Hash raw) (HashSet set) =
    Set.member raw set


toList : HashSet -> List Hash
toList (HashSet set) =
    set
        |> Set.toList
        |> List.map Hash


toggle : Hash -> HashSet -> HashSet
toggle (Hash raw) (HashSet set) =
    HashSet (Set.Extra.toggle raw set)
