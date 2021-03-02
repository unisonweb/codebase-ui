{-
   OpenDefinitions is the main data structure for working with definitions in
   the Workspace area. It supports holding Loading definitions and inserting
   definitions after another.

   It features a `focus` indicator and a before and after just like a Zipper,
   but is based on OrderedDicts. `focus` can be changed with `next` and `prev`.

   Invariants:
     It structurally can't hold the invariant that it should not contain
     duplicates, so this is instead enforced via the API; uniqueness is
     determined by Hash.
-}


module OpenDefinitions exposing
    ( HashIndexedDefinition
    , OpenDefinitions
    , empty
    , focus
    , fromDefinitions
    , hashes
    , init
    , insertWithFocus
    , insertWithFocusAfter
    , isEmpty
    , isFocused
    , map
    , mapToList
    , member
    , next
    , prev
    , remove
    , replace
    , singleton
    , toList
    )

import Definition exposing (Definition)
import Hash exposing (Hash)
import List
import List.Extra as ListE
import OrderedDict exposing (OrderedDict)
import RemoteData exposing (WebData)


type alias DefinitionDict =
    OrderedDict String (WebData Definition)


type alias HashIndexedDefinition =
    { hash : Hash
    , definition : WebData Definition
    }


{-| This technically allows multiple of the same definition across the 3 fields.
This is conceptionally not allowed and is enforced by the helper functions.
-}
type OpenDefinitions
    = Empty
    | OpenDefinitions
        { before : DefinitionDict
        , focus : HashIndexedDefinition
        , after : DefinitionDict
        }


init : Maybe Hash -> OpenDefinitions
init focused =
    case focused of
        Nothing ->
            Empty

        Just h ->
            singleton { hash = h, definition = RemoteData.NotAsked }


fromDefinitions :
    List HashIndexedDefinition
    -> HashIndexedDefinition
    -> List HashIndexedDefinition
    -> OpenDefinitions
fromDefinitions before focus_ after =
    OpenDefinitions
        { before = defDictFromList before
        , focus = focus_
        , after = defDictFromList after
        }


empty : OpenDefinitions
empty =
    Empty


isEmpty : OpenDefinitions -> Bool
isEmpty openDefinitions =
    case openDefinitions of
        Empty ->
            True

        OpenDefinitions _ ->
            False


singleton : HashIndexedDefinition -> OpenDefinitions
singleton hashIndexedDefinition =
    OpenDefinitions
        { before = OrderedDict.empty
        , focus = hashIndexedDefinition
        , after = OrderedDict.empty
        }



-- MODIFY


insertWithFocus : HashIndexedDefinition -> OpenDefinitions -> OpenDefinitions
insertWithFocus hashIndexedDefinition od =
    case od of
        Empty ->
            singleton hashIndexedDefinition

        OpenDefinitions _ ->
            let
                newBefore =
                    od
                        |> toList
                        |> List.map hashIndexedDefinitionToRawPair
                        |> OrderedDict.fromList
            in
            OpenDefinitions
                { before = newBefore
                , focus = hashIndexedDefinition
                , after = OrderedDict.empty
                }


{-| Insert after a Hash. If the Hash is not in OpenDefinitions, insert at the
end. If the element to insert already exists in OpenDefinitions, move it to
after the provided Hash
-}
insertWithFocusAfter :
    Hash
    -> HashIndexedDefinition
    -> OpenDefinitions
    -> OpenDefinitions
insertWithFocusAfter afterHash toInsert openDefinitions =
    case openDefinitions of
        Empty ->
            singleton toInsert

        OpenDefinitions _ ->
            if member afterHash openDefinitions then
                let
                    insertAfter def =
                        if Hash.equals def.hash afterHash then
                            [ def, toInsert ]

                        else
                            [ def ]

                    make ( before, afterInclusive ) =
                        OpenDefinitions
                            { before = defDictFromList before
                            , focus = toInsert
                            , after = defDictFromList (List.drop 1 afterInclusive)
                            }
                in
                openDefinitions
                    |> toList
                    |> List.concatMap insertAfter
                    |> ListE.splitWhen (\d -> Hash.equals toInsert.hash d.hash)
                    |> Maybe.map make
                    |> Maybe.withDefault (singleton toInsert)

            else
                insertWithFocus toInsert openDefinitions


replace : Hash -> WebData Definition -> OpenDefinitions -> OpenDefinitions
replace hash newDef ods =
    let
        replaceMatching d =
            if Hash.equals d.hash hash then
                newDef

            else
                d.definition
    in
    map replaceMatching ods


remove : Hash -> OpenDefinitions -> OpenDefinitions
remove hash od =
    case od of
        Empty ->
            Empty

        OpenDefinitions data ->
            if Hash.equals hash data.focus.hash then
                let
                    rightBeforeFocus =
                        data.before
                            |> OrderedDict.toList
                            |> ListE.last

                    rightAfterFocus =
                        data.after
                            |> OrderedDict.toList
                            |> List.head
                in
                case rightAfterFocus of
                    Just ( rawHash, d ) ->
                        OpenDefinitions
                            { before = data.before
                            , focus = HashIndexedDefinition (Hash.fromString rawHash) d
                            , after = OrderedDict.remove rawHash data.after
                            }

                    Nothing ->
                        case rightBeforeFocus of
                            Just ( rawHash, d ) ->
                                OpenDefinitions
                                    { before = OrderedDict.remove rawHash data.before
                                    , focus = HashIndexedDefinition (Hash.fromString rawHash) d
                                    , after = data.after
                                    }

                            Nothing ->
                                Empty

            else
                let
                    rawHash =
                        Hash.toString hash
                in
                OpenDefinitions
                    { before = OrderedDict.remove rawHash data.before
                    , focus = data.focus
                    , after = OrderedDict.remove rawHash data.after
                    }



-- QUERY


member : Hash -> OpenDefinitions -> Bool
member hash od =
    case od of
        Empty ->
            False

        OpenDefinitions data ->
            let
                rawHash =
                    Hash.toString hash
            in
            Hash.equals data.focus.hash hash || OrderedDict.member rawHash data.before || OrderedDict.member rawHash data.after


hashes : OpenDefinitions -> List Hash
hashes =
    toList >> List.map .hash



-- Focus


focus : OpenDefinitions -> Maybe HashIndexedDefinition
focus openDefinitions =
    case openDefinitions of
        Empty ->
            Nothing

        OpenDefinitions data ->
            Just data.focus


isFocused : Hash -> OpenDefinitions -> Bool
isFocused hash openDefinitions =
    openDefinitions
        |> focus
        |> Maybe.map (.hash >> Hash.equals hash)
        |> Maybe.withDefault False


next : OpenDefinitions -> OpenDefinitions
next openDefinitions =
    case openDefinitions of
        Empty ->
            Empty

        OpenDefinitions data ->
            case OrderedDict.toList data.after of
                [] ->
                    openDefinitions

                newFocus :: rest ->
                    OpenDefinitions
                        { before = OrderedDict.insert (Hash.toString data.focus.hash) data.focus.definition data.before
                        , focus = hashIndexedDefinitionFromRawPair newFocus
                        , after = OrderedDict.fromList rest
                        }


prev : OpenDefinitions -> OpenDefinitions
prev openDefinitions =
    case openDefinitions of
        Empty ->
            Empty

        OpenDefinitions data ->
            let
                after =
                    data.after
                        |> OrderedDict.toList
                        |> (\l -> hashIndexedDefinitionToRawPair data.focus :: l)
                        |> OrderedDict.fromList

                before =
                    data.before
                        |> OrderedDict.toList
                        |> ListE.unconsLast
            in
            case before of
                Nothing ->
                    openDefinitions

                Just ( newFocus, newBefore ) ->
                    OpenDefinitions
                        { before = OrderedDict.fromList newBefore
                        , focus = hashIndexedDefinitionFromRawPair newFocus
                        , after = after
                        }



-- TRANFORM


map :
    (HashIndexedDefinition -> WebData Definition)
    -> OpenDefinitions
    -> OpenDefinitions
map f openDefinitions =
    case openDefinitions of
        Empty ->
            Empty

        OpenDefinitions data ->
            let
                mapper rawHash d =
                    f (hashIndexedDefinitionFromRawHash rawHash d)

                newFocus =
                    { hash = data.focus.hash
                    , definition = f data.focus
                    }
            in
            OpenDefinitions
                { before = OrderedDict.map mapper data.before
                , focus = newFocus
                , after = OrderedDict.map mapper data.after
                }


mapToList : (HashIndexedDefinition -> Bool -> a) -> OpenDefinitions -> List a
mapToList f openDefinitions =
    case openDefinitions of
        Empty ->
            []

        OpenDefinitions data ->
            let
                before =
                    data.before
                        |> OrderedDict.toList
                        |> List.map (\pair -> f (hashIndexedDefinitionFromRawPair pair) False)

                after =
                    data.after
                        |> OrderedDict.toList
                        |> List.map (\pair -> f (hashIndexedDefinitionFromRawPair pair) False)
            in
            before ++ (f data.focus True :: after)


toList : OpenDefinitions -> List HashIndexedDefinition
toList openDefinitions =
    case openDefinitions of
        Empty ->
            []

        OpenDefinitions data ->
            let
                toList_ =
                    OrderedDict.toList >> List.map hashIndexedDefinitionFromRawPair
            in
            toList_ data.before ++ (data.focus :: toList_ data.after)



-- INTERNAL HELPERS


hashIndexedDefinitionFromRawPair :
    ( String, WebData Definition )
    -> HashIndexedDefinition
hashIndexedDefinitionFromRawPair ( rawHash, def ) =
    hashIndexedDefinitionFromRawHash rawHash def


hashIndexedDefinitionFromRawHash :
    String
    -> WebData Definition
    -> HashIndexedDefinition
hashIndexedDefinitionFromRawHash rawHash def =
    HashIndexedDefinition (Hash.fromString rawHash) def


hashIndexedDefinitionToRawPair :
    HashIndexedDefinition
    -> ( String, WebData Definition )
hashIndexedDefinitionToRawPair hashIndexedDefinition =
    ( Hash.toString hashIndexedDefinition.hash, hashIndexedDefinition.definition )


defDictFromList : List HashIndexedDefinition -> DefinitionDict
defDictFromList =
    List.map hashIndexedDefinitionToRawPair >> OrderedDict.fromList
