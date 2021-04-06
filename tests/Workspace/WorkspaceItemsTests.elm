module Workspace.WorkspaceItemsTests exposing (..)

import Expect
import Hash
import HashQualified exposing (HashQualified(..))
import Test exposing (..)
import Workspace.Reference as Reference exposing (Reference(..))
import Workspace.WorkspaceItems as WorkspaceItems exposing (..)



-- MODIFY


insertWithFocus : Test
insertWithFocus =
    let
        result =
            WorkspaceItems.insertWithFocus WorkspaceItems.empty term

        currentFocusedRef =
            getFocusedRef result
    in
    describe "WorkspaceItems.insertWithFocus"
        [ test "Inserts the term" <|
            \_ ->
                Expect.true "term is a member" (WorkspaceItems.member result termRef)
        , test "Sets focus" <|
            \_ ->
                Expect.true "Has focus" (Maybe.withDefault False <| Maybe.map (\r -> r == termRef) currentFocusedRef)
        ]


insertWithFocusAfter : Test
insertWithFocusAfter =
    let
        afterRef =
            TermReference (HashOnly (Hash.fromString "#a"))

        toInsert =
            term

        expected =
            [ Loading (TermReference (HashOnly (Hash.fromString "#a")))
            , Loading termRef
            , Loading (TermReference (HashOnly (Hash.fromString "#b")))
            , Loading (TermReference (HashOnly (Hash.fromString "#focus")))
            , Loading (TermReference (HashOnly (Hash.fromString "#c")))
            , Loading (TermReference (HashOnly (Hash.fromString "#d")))
            ]

        inserted =
            WorkspaceItems.insertWithFocusAfter workspaceItems afterRef toInsert

        currentFocusedRef =
            getFocusedRef inserted
    in
    describe "WorkspaceItems.insertWithFocusAfter"
        [ test "Inserts after the the 'after hash'" <|
            \_ ->
                Expect.equal expected (WorkspaceItems.toList inserted)
        , test "When inserted, the new element has focus" <|
            \_ ->
                Expect.true "Has focus" (Maybe.withDefault False <| Maybe.map (\r -> r == reference toInsert) currentFocusedRef)
        , test "When the 'after hash' is not present, insert at the end" <|
            \_ ->
                let
                    atEnd =
                        [ Loading (TermReference (HashOnly (Hash.fromString "#a")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#b")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#focus")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#c")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#d")))
                        , Loading (reference toInsert)
                        ]

                    result =
                        toInsert
                            |> WorkspaceItems.insertWithFocusAfter workspaceItems (TermReference (HashOnly (Hash.fromString "#notfound")))
                            |> WorkspaceItems.toList
                in
                Expect.equal atEnd result
        ]


replace : Test
replace =
    describe "WorkspaceItems.replace"
        [ test "Can replace element in 'before' focus" <|
            \_ ->
                let
                    newItem =
                        Failure (TermReference (HashOnly (Hash.fromString "#b"))) (Error "err")

                    expected =
                        [ Loading (TermReference (HashOnly (Hash.fromString "#a")))
                        , newItem
                        , Loading (TermReference (HashOnly (Hash.fromString "#focus")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#c")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#d")))
                        ]

                    result =
                        newItem
                            |> WorkspaceItems.replace workspaceItems (TermReference (HashOnly (Hash.fromString "#b")))
                            |> WorkspaceItems.toList
                in
                Expect.equal expected result
        , test "Can replace element in focus" <|
            \_ ->
                let
                    newItem =
                        Failure (TermReference (HashOnly (Hash.fromString "#focus"))) (Error "err")

                    expected =
                        [ Loading (TermReference (HashOnly (Hash.fromString "#a")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#b")))
                        , newItem
                        , Loading (TermReference (HashOnly (Hash.fromString "#c")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#d")))
                        ]

                    result =
                        newItem
                            |> WorkspaceItems.replace workspaceItems (TermReference (HashOnly (Hash.fromString "#focus")))
                            |> WorkspaceItems.toList
                in
                Expect.equal expected result
        , test "Can replace element in 'after' focus" <|
            \_ ->
                let
                    newItem =
                        Failure (TermReference (HashOnly (Hash.fromString "#d"))) (Error "err")

                    expected =
                        [ Loading (TermReference (HashOnly (Hash.fromString "#a")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#b")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#focus")))
                        , Loading (TermReference (HashOnly (Hash.fromString "#c")))
                        , newItem
                        ]

                    result =
                        newItem
                            |> WorkspaceItems.replace workspaceItems (TermReference (HashOnly (Hash.fromString "#d")))
                            |> WorkspaceItems.toList
                in
                Expect.equal expected result
        ]


remove : Test
remove =
    describe "WorkspaceItems.remove"
        [ test "Returns original when trying to remove a missing Hash" <|
            \_ ->
                let
                    expected =
                        WorkspaceItems.toList workspaceItems

                    result =
                        notFoundRef
                            |> WorkspaceItems.remove workspaceItems
                            |> WorkspaceItems.toList
                in
                Expect.equal expected result
        , test "Removes the element" <|
            \_ ->
                let
                    toRemove =
                        TermReference (HashOnly (Hash.fromString "#a"))

                    result =
                        WorkspaceItems.remove workspaceItems toRemove
                in
                Expect.false "#a is removed" (WorkspaceItems.member result toRemove)
        , test "When the element to remove is focused, remove the element and change focus to right after it" <|
            \_ ->
                let
                    toRemove =
                        TermReference (HashOnly (Hash.fromString "#focus"))

                    expectedNewFocus =
                        TermReference (HashOnly (Hash.fromString "#c"))

                    result =
                        WorkspaceItems.remove workspaceItems toRemove
                in
                Expect.true "#focus is removed and #c has focus" (not (WorkspaceItems.member result toRemove) && WorkspaceItems.isFocused result expectedNewFocus)
        , test "When the element to remove is focused and there are no elements after, remove the element and change focus to right before it" <|
            \_ ->
                let
                    toRemove =
                        TermReference (HashOnly (Hash.fromString "#focus"))

                    expectedNewFocus =
                        TermReference (HashOnly (Hash.fromString "#b"))

                    result =
                        WorkspaceItems.remove (WorkspaceItems.fromItems before focused []) toRemove
                in
                Expect.true "#focus is removed and #b has focus" (not (WorkspaceItems.member result toRemove) && WorkspaceItems.isFocused result expectedNewFocus)
        , test "When the element to remove is focused there are no other elements, it returns Empty" <|
            \_ ->
                let
                    result =
                        WorkspaceItems.remove (WorkspaceItems.singleton term) (reference term)
                in
                Expect.true "Definition is empty" (WorkspaceItems.isEmpty result)
        ]



-- QUERY


member : Test
member =
    let
        items =
            WorkspaceItems.singleton term
    in
    describe "WorkspaceItems.member"
        [ test "Returns true for a ref housed within" <|
            \_ ->
                Expect.true "item is a member" (WorkspaceItems.member items termRef)
        , test "Returns false for a ref *not* housed within" <|
            \_ ->
                Expect.false "item is *not* a member" (WorkspaceItems.member items notFoundRef)
        ]



-- FOCUS


next : Test
next =
    describe "WorkspaceItems.next"
        [ test "moves focus to the next element" <|
            \_ ->
                let
                    result =
                        workspaceItems
                            |> WorkspaceItems.next
                            |> getFocusedRef
                            |> Maybe.map Reference.toString
                in
                Expect.equal (Just "term/#c") result
        , test "keeps focus if no elements after" <|
            \_ ->
                let
                    result =
                        WorkspaceItems.fromItems before focused []
                            |> WorkspaceItems.next
                            |> getFocusedRef
                            |> Maybe.map Reference.toString
                in
                Expect.equal (Just "term/#focus") result
        ]


prev : Test
prev =
    describe "WorkspaceItems.prev"
        [ test "moves focus to the prev element" <|
            \_ ->
                let
                    result =
                        workspaceItems
                            |> WorkspaceItems.prev
                            |> getFocusedRef
                            |> Maybe.map Reference.toString
                in
                Expect.equal (Just "term/#b") result
        , test "keeps focus if no elements before" <|
            \_ ->
                let
                    result =
                        WorkspaceItems.fromItems [] focused after
                            |> WorkspaceItems.prev
                            |> getFocusedRef
                            |> Maybe.map Reference.toString
                in
                Expect.equal (Just "term/#focus") result
        ]



-- MAP


map : Test
map =
    describe "WorkspaceItems.map"
        [ test "Maps definitions" <|
            \_ ->
                let
                    result =
                        workspaceItems
                            |> WorkspaceItems.map (\i -> Failure (reference i) (Error "err"))
                            |> WorkspaceItems.toList

                    expected =
                        [ Failure (TermReference (HashOnly (Hash.fromString "#a"))) (Error "err")
                        , Failure (TermReference (HashOnly (Hash.fromString "#b"))) (Error "err")
                        , Failure (TermReference (HashOnly (Hash.fromString "#focus"))) (Error "err")
                        , Failure (TermReference (HashOnly (Hash.fromString "#c"))) (Error "err")
                        , Failure (TermReference (HashOnly (Hash.fromString "#d"))) (Error "err")
                        ]
                in
                Expect.equal expected result
        ]


mapToList : Test
mapToList =
    describe "WorkspaceItems.mapToList"
        [ test "Maps definitions" <|
            \_ ->
                let
                    result =
                        workspaceItems
                            |> WorkspaceItems.mapToList (\i _ -> Reference.toString (reference i))

                    expected =
                        [ "term/#a", "term/#b", "term/#focus", "term/#c", "term/#d" ]
                in
                Expect.equal expected result
        ]



-- TEST HELPERS


termRef : Reference
termRef =
    TermReference hashQualified


notFoundRef : Reference
notFoundRef =
    TermReference (HashOnly (Hash.fromString "#notfound"))


hashQualified : HashQualified
hashQualified =
    HashOnly (Hash.fromString "#testhash")


term : WorkspaceItem
term =
    Loading termRef


before : List WorkspaceItem
before =
    [ Loading (TermReference (HashOnly (Hash.fromString "#a")))
    , Loading (TermReference (HashOnly (Hash.fromString "#b")))
    ]


focused : WorkspaceItem
focused =
    Loading (TermReference (HashOnly (Hash.fromString "#focus")))


after : List WorkspaceItem
after =
    [ Loading (TermReference (HashOnly (Hash.fromString "#c")))
    , Loading (TermReference (HashOnly (Hash.fromString "#d")))
    ]


workspaceItems : WorkspaceItems
workspaceItems =
    WorkspaceItems.fromItems before focused after


getFocusedRef : WorkspaceItems -> Maybe Reference
getFocusedRef =
    WorkspaceItems.focus >> Maybe.map WorkspaceItems.reference
