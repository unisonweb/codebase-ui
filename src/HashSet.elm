module HashSet exposing (HashSet, empty, insert, member, remove, toList, toggle)

import Hash exposing (Hash)
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
insert h (HashSet set) =
    HashSet (Set.insert (Hash.toString h) set)


remove : Hash -> HashSet -> HashSet
remove h (HashSet set) =
    HashSet (Set.remove (Hash.toString h) set)


member : Hash -> HashSet -> Bool
member h (HashSet set) =
    Set.member (Hash.toString h) set


toList : HashSet -> List Hash
toList (HashSet set) =
    set
        |> Set.toList
        |> List.map Hash.fromString


toggle : Hash -> HashSet -> HashSet
toggle h (HashSet set) =
    HashSet (Set.Extra.toggle (Hash.toString h) set)
