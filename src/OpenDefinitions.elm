module OpenDefinitions exposing (OpenDefinitions, definitions, empty, fromList, hashes, insert, insertList, loading, remove, toList, update)

import Definition exposing (Definition)
import Hash exposing (Hash)
import OrderedDict exposing (OrderedDict)
import RemoteData exposing (WebData)



{-
   OpenDefinitions is the main data structure for working with definitions in
   the Workspace area. It wraps an OrderedDirect (Hash to Definition), but is
   opaque to that underlying data structure and only exposes a few key APIs,
   including a few convenience functions for working with, loading and opening
   Definitions.
-}


type OpenDefinitions
    = OpenDefinitions (OrderedDict String (WebData Definition))



-- Build


empty : OpenDefinitions
empty =
    OpenDefinitions OrderedDict.empty


loading : List Hash -> OpenDefinitions
loading list =
    list
        |> List.map (\h -> ( h, RemoteData.Loading ))
        |> fromList


fromList : List ( Hash, WebData Definition ) -> OpenDefinitions
fromList list =
    list
        |> List.map (\( h, d ) -> ( Hash.toString h, d ))
        |> OrderedDict.fromList
        |> OpenDefinitions


update : Hash -> (Maybe (WebData Definition) -> Maybe (WebData Definition)) -> OpenDefinitions -> OpenDefinitions
update h f (OpenDefinitions dict) =
    OpenDefinitions (OrderedDict.update (Hash.toString h) f dict)


insert : Hash -> WebData Definition -> OpenDefinitions -> OpenDefinitions
insert h definition (OpenDefinitions dict) =
    OpenDefinitions (OrderedDict.insert (Hash.toString h) definition dict)


insertBefore : Hash -> Hash -> WebData Definition -> OpenDefinitions -> OpenDefinitions
insertBefore beforeHash insertHash definition (OpenDefinitions dict) =
    -- TODO
    OpenDefinitions dict


insertList : List ( Hash, WebData Definition ) -> OpenDefinitions -> OpenDefinitions
insertList list openDefinitions =
    let
        inserter ( h, d ) acc =
            insert h d acc
    in
    List.foldl inserter openDefinitions list


remove : Hash -> OpenDefinitions -> OpenDefinitions
remove h (OpenDefinitions dict) =
    OpenDefinitions (OrderedDict.remove (Hash.toString h) dict)



-- Query


get : Hash -> OpenDefinitions -> Maybe (WebData Definition)
get h (OpenDefinitions dict) =
    OrderedDict.get (Hash.toString h) dict



-- Conversions


hashes : OpenDefinitions -> List Hash
hashes (OpenDefinitions dict) =
    dict
        |> OrderedDict.keys
        |> List.map Hash.fromString


definitions : OpenDefinitions -> List (WebData Definition)
definitions (OpenDefinitions dict) =
    OrderedDict.values dict


toList : OpenDefinitions -> List ( Hash, WebData Definition )
toList (OpenDefinitions dict) =
    dict
        |> OrderedDict.toList
        |> List.map (\( h, d ) -> ( Hash.fromString h, d ))
