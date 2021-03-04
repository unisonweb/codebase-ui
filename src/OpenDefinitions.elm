{-
   OpenDefinitions is the main data structure for working with definitions in
   the Workspace area. It supports holding Loading definitions and inserting
   definitions after another.

   It features a `focus` indicator and a before and after just like a Zipper.
   `focus` can be changed with `next` and `prev`.

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
import RemoteData exposing (WebData)


{-| This technically allows multiple of the same definition across the 3 fields.
This is conceptionally not allowed and is enforced by the helper functions.
-}
type OpenDefinitions
    = Empty
    | OpenDefinitions
        { before : List HashIndexedDefinition
        , focus : HashIndexedDefinition
        , after : List HashIndexedDefinition
        }


type alias HashIndexedDefinition =
    { hash : Hash
    , definition : WebData Definition
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
    OpenDefinitions { before = before, focus = focus_, after = after }


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
    OpenDefinitions { before = [], focus = hashIndexedDefinition, after = [] }



-- MODIFY


insertWithFocus : HashIndexedDefinition -> OpenDefinitions -> OpenDefinitions
insertWithFocus hid od =
    OpenDefinitions { before = toList od, focus = hid, after = [] }


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
                            { before = before
                            , focus = toInsert
                            , after = List.drop 1 afterInclusive
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
            let
                without h list =
                    ListE.filterNot (.hash >> Hash.equals h) list
            in
            if Hash.equals hash data.focus.hash then
                let
                    rightBeforeFocus =
                        ListE.last data.before

                    rightAfterFocus =
                        List.head data.after
                in
                case rightAfterFocus of
                    Just hid ->
                        OpenDefinitions
                            { before = data.before
                            , focus = hid
                            , after = without hid.hash data.after
                            }

                    Nothing ->
                        case rightBeforeFocus of
                            Just hid ->
                                OpenDefinitions
                                    { before = without hid.hash data.before
                                    , focus = hid
                                    , after = data.after
                                    }

                            Nothing ->
                                Empty

            else
                OpenDefinitions
                    { before = without hash data.before
                    , focus = data.focus
                    , after = without hash data.after
                    }



-- QUERY


member : Hash -> OpenDefinitions -> Bool
member hash od =
    od |> hashes |> List.member hash


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
            case data.after of
                [] ->
                    openDefinitions

                newFocus :: rest ->
                    OpenDefinitions
                        { before = data.before ++ [ data.focus ]
                        , focus = newFocus
                        , after = rest
                        }


prev : OpenDefinitions -> OpenDefinitions
prev openDefinitions =
    case openDefinitions of
        Empty ->
            Empty

        OpenDefinitions data ->
            case ListE.unconsLast data.before of
                Nothing ->
                    openDefinitions

                Just ( newFocus, newBefore ) ->
                    OpenDefinitions
                        { before = newBefore
                        , focus = newFocus
                        , after = data.focus :: data.after
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
                f_ hid =
                    { hash = hid.hash, definition = f hid }
            in
            OpenDefinitions
                { before = List.map f_ data.before
                , focus = f_ data.focus
                , after = List.map f_ data.after
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
                        |> List.map (\hid -> f hid False)

                after =
                    data.after
                        |> List.map (\hid -> f hid False)
            in
            before ++ (f data.focus True :: after)


{-| Convert the open definitions to a list, looses the focus indicator
-}
toList : OpenDefinitions -> List HashIndexedDefinition
toList openDefinitions =
    case openDefinitions of
        Empty ->
            []

        OpenDefinitions data ->
            data.before ++ (data.focus :: data.after)
