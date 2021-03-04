module OpenDefinitionsTests exposing (..)

import Definition exposing (Definition)
import Expect
import Hash exposing (Hash)
import OpenDefinitions exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Test exposing (..)



-- MODIFY


insertWithFocus : Test
insertWithFocus =
    let
        result =
            OpenDefinitions.insertWithFocus (HashIndexedDefinition hash definitionData) OpenDefinitions.empty

        currentFocusedHash =
            getFocusedHash result
    in
    describe "OpenDefinitions.insertWithFocus"
        [ test "Inserts the definition" <|
            \_ ->
                Expect.true "Definition is a member" (OpenDefinitions.member hash result)
        , test "Sets focus" <|
            \_ ->
                Expect.true "Has focus" (Maybe.withDefault False <| Maybe.map (Hash.equals hash) currentFocusedHash)
        ]


insertWithFocusAfter : Test
insertWithFocusAfter =
    let
        afterHash =
            Hash.fromString "#a"

        toInsert =
            HashIndexedDefinition hash definitionData

        expected =
            [ ( "#a", NotAsked )
            , ( Hash.toString hash, NotAsked )
            , ( "#b", NotAsked )
            , ( "#focus", NotAsked )
            , ( "#c", NotAsked )
            , ( "#d", NotAsked )
            ]

        inserted =
            OpenDefinitions.insertWithFocusAfter afterHash toInsert openDefinitions

        currentFocusedHash =
            getFocusedHash inserted
    in
    describe "OpenDefinitions.insertWithFocusAfter"
        [ test "Inserts after the the 'after hash'" <|
            \_ ->
                Expect.equal expected (toComparableList inserted)
        , test "When inserted, the new element has focus" <|
            \_ ->
                Expect.true "Has focus" (Maybe.withDefault False <| Maybe.map (Hash.equals toInsert.hash) currentFocusedHash)
        , test "When the 'after hash' is not present, insert at the end" <|
            \_ ->
                let
                    atEnd =
                        [ ( "#a", NotAsked )
                        , ( "#b", NotAsked )
                        , ( "#focus", NotAsked )
                        , ( "#c", NotAsked )
                        , ( "#d", NotAsked )
                        , ( Hash.toString toInsert.hash, NotAsked )
                        ]

                    result =
                        openDefinitions
                            |> OpenDefinitions.insertWithFocusAfter (Hash.fromString "#notfound") toInsert
                            |> toComparableList
                in
                Expect.equal atEnd result
        ]


replace : Test
replace =
    describe "OpenDefinitions.replace"
        [ test "Can replace element in 'before' focus" <|
            \_ ->
                let
                    expected =
                        [ ( "#a", NotAsked )
                        , ( "#b", Loading )
                        , ( "#focus", NotAsked )
                        , ( "#c", NotAsked )
                        , ( "#d", NotAsked )
                        ]

                    result =
                        openDefinitions
                            |> OpenDefinitions.replace (Hash.fromString "#b") Loading
                            |> toComparableList
                in
                Expect.equal expected result
        , test "Can replace element in focus" <|
            \_ ->
                let
                    expected =
                        [ ( "#a", NotAsked )
                        , ( "#b", NotAsked )
                        , ( "#focus", Loading )
                        , ( "#c", NotAsked )
                        , ( "#d", NotAsked )
                        ]

                    result =
                        openDefinitions
                            |> OpenDefinitions.replace (Hash.fromString "#focus") Loading
                            |> toComparableList
                in
                Expect.equal expected result
        , test "Can replace element in 'after' focus" <|
            \_ ->
                let
                    expected =
                        [ ( "#a", NotAsked )
                        , ( "#b", NotAsked )
                        , ( "#focus", NotAsked )
                        , ( "#c", NotAsked )
                        , ( "#d", Loading )
                        ]

                    result =
                        openDefinitions
                            |> OpenDefinitions.replace (Hash.fromString "#d") Loading
                            |> toComparableList
                in
                Expect.equal expected result
        ]


remove : Test
remove =
    describe "OpenDefinitions.remove"
        [ test "Returns original when trying to remove a missing Hash" <|
            \_ ->
                let
                    expected =
                        toComparableList openDefinitions

                    result =
                        openDefinitions
                            |> OpenDefinitions.remove (Hash.fromString "#notfound")
                            |> toComparableList
                in
                Expect.equal expected result
        , test "Removes the element" <|
            \_ ->
                let
                    toRemove =
                        Hash.fromString "#a"

                    result =
                        OpenDefinitions.remove toRemove openDefinitions
                in
                Expect.false "#a is removed" (OpenDefinitions.member toRemove result)
        , test "When the element to remove is focused, remove the element and change focus to right after it" <|
            \_ ->
                let
                    toRemove =
                        Hash.fromString "#focus"

                    expectedNewFocus =
                        Hash.fromString "#c"

                    result =
                        OpenDefinitions.remove toRemove openDefinitions
                in
                Expect.true "#focus is removed and #c has focus" (not (OpenDefinitions.member toRemove result) && OpenDefinitions.isFocused expectedNewFocus result)
        , test "When the element to remove is focused and there are no elements after, remove the element and change focus to right before it" <|
            \_ ->
                let
                    toRemove =
                        Hash.fromString "#focus"

                    expectedNewFocus =
                        Hash.fromString "#b"

                    result =
                        OpenDefinitions.remove toRemove (OpenDefinitions.fromDefinitions before focused [])
                in
                Expect.true "#focus is removed and #b has focus" (not (OpenDefinitions.member toRemove result) && OpenDefinitions.isFocused expectedNewFocus result)
        , test "When the element to remove is focused there are no other elements, it returns Empty" <|
            \_ ->
                let
                    result =
                        HashIndexedDefinition hash definitionData
                            |> OpenDefinitions.singleton
                            |> OpenDefinitions.remove hash
                in
                Expect.true "Definition is empty" (OpenDefinitions.isEmpty result)
        ]



-- QUERY


member : Test
member =
    let
        definition =
            HashIndexedDefinition hash definitionData

        openDefs =
            OpenDefinitions.singleton definition

        notFoundHash =
            Hash.fromString "#notfound"
    in
    describe "OpenDefinitions.member"
        [ test "Returns true for a hash housed within" <|
            \_ ->
                Expect.true "Definition is a member" (OpenDefinitions.member hash openDefs)
        , test "Returns false for a hash *not* housed within" <|
            \_ ->
                Expect.false "Definition is *not* a member" (OpenDefinitions.member notFoundHash openDefs)
        ]



-- FOCUS


next : Test
next =
    describe "OpenDefinitions.next"
        [ test "moves focus to the next element" <|
            \_ ->
                let
                    result =
                        openDefinitions
                            |> OpenDefinitions.next
                            |> getFocusedHash
                            |> Maybe.map Hash.toString
                in
                Expect.equal (Just "#c") result
        , test "keeps focus if no elements after" <|
            \_ ->
                let
                    result =
                        OpenDefinitions.fromDefinitions before focused []
                            |> OpenDefinitions.next
                            |> getFocusedHash
                            |> Maybe.map Hash.toString
                in
                Expect.equal (Just "#focus") result
        ]


prev : Test
prev =
    describe "OpenDefinitions.prev"
        [ test "moves focus to the next element" <|
            \_ ->
                let
                    result =
                        openDefinitions
                            |> OpenDefinitions.prev
                            |> getFocusedHash
                            |> Maybe.map Hash.toString
                in
                Expect.equal (Just "#b") result
        , test "keeps focus if no elements before" <|
            \_ ->
                let
                    result =
                        OpenDefinitions.fromDefinitions [] focused after
                            |> OpenDefinitions.prev
                            |> getFocusedHash
                            |> Maybe.map Hash.toString
                in
                Expect.equal (Just "#focus") result
        ]



-- MAP


map : Test
map =
    describe "OpenDefinitions.map"
        [ test "Maps definitions" <|
            \_ ->
                let
                    result =
                        openDefinitions
                            |> OpenDefinitions.map (\_ -> Loading)
                            |> toComparableList

                    expected =
                        [ ( "#a", Loading )
                        , ( "#b", Loading )
                        , ( "#focus", Loading )
                        , ( "#c", Loading )
                        , ( "#d", Loading )
                        ]
                in
                Expect.equal expected result
        ]


mapToList : Test
mapToList =
    describe "OpenDefinitions.mapToList"
        [ test "Maps definitions" <|
            \_ ->
                let
                    result =
                        openDefinitions
                            |> OpenDefinitions.mapToList (\hid _ -> Hash.toString hid.hash)

                    expected =
                        [ "#a", "#b", "#focus", "#c", "#d" ]
                in
                Expect.equal expected result
        ]



-- TEST HELPERS


hash : Hash
hash =
    Hash.fromString "#testhash"


definitionData : WebData Definition
definitionData =
    -- The details of a definition doesn't really matter in a test setting
    RemoteData.NotAsked


before : List HashIndexedDefinition
before =
    [ HashIndexedDefinition (Hash.fromString "#a") NotAsked
    , HashIndexedDefinition (Hash.fromString "#b") NotAsked
    ]


focused : HashIndexedDefinition
focused =
    HashIndexedDefinition (Hash.fromString "#focus") NotAsked


after : List HashIndexedDefinition
after =
    [ HashIndexedDefinition (Hash.fromString "#c") NotAsked
    , HashIndexedDefinition (Hash.fromString "#d") NotAsked
    ]


openDefinitions : OpenDefinitions
openDefinitions =
    OpenDefinitions.fromDefinitions before focused after


toComparableList : OpenDefinitions -> List ( String, WebData Definition )
toComparableList =
    OpenDefinitions.toList
        >> List.map (\d -> ( Hash.toString d.hash, d.definition ))


getFocusedHash : OpenDefinitions -> Maybe Hash
getFocusedHash =
    OpenDefinitions.focus >> Maybe.map .hash
