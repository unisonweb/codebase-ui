module OpenDefinitions exposing
    ( OpenDefinitions
    , definitions
    , empty
    , fromList
    , get
    , hashes
    , insert
    , insertAfter
    , loading
    , member
    , remove
    , replace
    , replaceItems
    , toList
    , update
    )

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



-- BUILD


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
        |> List.map (Tuple.mapFirst Hash.toString)
        |> OrderedDict.fromList
        |> OpenDefinitions


update :
    Hash
    -> (Maybe (WebData Definition) -> Maybe (WebData Definition))
    -> OpenDefinitions
    -> OpenDefinitions
update h f (OpenDefinitions dict) =
    OpenDefinitions (OrderedDict.update (Hash.toString h) f dict)


{-| Replace existing item |
-}
replace : Hash -> WebData Definition -> OpenDefinitions -> OpenDefinitions
replace h def openDefinition =
    update h (Maybe.map (always def)) openDefinition


{-| Replace existing items in OpenDefinitions |
-}
replaceItems :
    List ( Hash, WebData Definition )
    -> OpenDefinitions
    -> OpenDefinitions
replaceItems items openDefinitions =
    let
        replacer ( h, d ) acc =
            replace h d acc
    in
    List.foldl replacer openDefinitions items


remove : Hash -> OpenDefinitions -> OpenDefinitions
remove h (OpenDefinitions dict) =
    OpenDefinitions (OrderedDict.remove (Hash.toString h) dict)


insert : ( Hash, WebData Definition ) -> OpenDefinitions -> OpenDefinitions
insert ( h, definition ) (OpenDefinitions dict) =
    OpenDefinitions (OrderedDict.insert (Hash.toString h) definition dict)


{-| Insert after a Hash. If the Hash is not in OpenDefinitions, insert at the
end. If the element to insert already exists in OpenDefinitions, move it to
after the provided Hash |
-}
insertAfter : Hash -> ( Hash, WebData Definition ) -> OpenDefinitions -> OpenDefinitions
insertAfter afterHash (( hashToInsert, _ ) as definitionRow) openDefinitions =
    let
        after (( h, _ ) as elem) =
            if Hash.equals afterHash h then
                [ elem, definitionRow ]

            else
                [ elem ]

        insertAfter_ defs =
            defs
                |> toList
                |> List.filter (\( h, _ ) -> not (Hash.equals h hashToInsert))
                |> List.concatMap after
                |> fromList
    in
    if member afterHash openDefinitions then
        insertAfter_ openDefinitions

    else
        insert definitionRow openDefinitions



-- QUERY


member : Hash -> OpenDefinitions -> Bool
member hash (OpenDefinitions dict) =
    OrderedDict.member (Hash.toString hash) dict


get : Hash -> OpenDefinitions -> Maybe (WebData Definition)
get h (OpenDefinitions dict) =
    OrderedDict.get (Hash.toString h) dict



-- CONVERSIONS


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
        |> List.map (Tuple.mapFirst Hash.fromString)
