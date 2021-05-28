module Workspace.WorkspaceItemsTests exposing (..)

import Definition.Reference as Reference exposing (Reference(..))
import Expect
import HashQualified exposing (HashQualified(..))
import Http exposing (Error(..))
import Test exposing (..)
import Workspace.WorkspaceItem exposing (WorkspaceItem(..), reference)
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
            termRefFromStr "#a"

        toInsert =
            term

        expected =
            [ Loading (termRefFromStr "#a")
            , Loading termRef
            , Loading (termRefFromStr "#b")
            , Loading (termRefFromStr "#focus")
            , Loading (termRefFromStr "#c")
            , Loading (termRefFromStr "#d")
            ]

        inserted =
            WorkspaceItems.insertWithFocusAfter workspaceItems afterRef toInsert

        currentFocusedRef =
            getFocusedRef inserted
    in
    describe "WorkspaceItems.insertWithFocusAfter"
        [ test "Inserts after the 'after ref'" <|
            \_ ->
                Expect.equal expected (WorkspaceItems.toList inserted)
        , test "When inserted, the new element has focus" <|
            \_ ->
                Expect.true "Has focus" (Maybe.withDefault False <| Maybe.map (\r -> r == reference toInsert) currentFocusedRef)
        , test "When the 'after ref' is not present, insert at the end" <|
            \_ ->
                let
                    atEnd =
                        [ Loading (termRefFromStr "#a")
                        , Loading (termRefFromStr "#b")
                        , Loading (termRefFromStr "#focus")
                        , Loading (termRefFromStr "#c")
                        , Loading (termRefFromStr "#d")
                        , Loading (reference toInsert)
                        ]

                    result =
                        toInsert
                            |> WorkspaceItems.insertWithFocusAfter workspaceItems notFoundRef
                            |> WorkspaceItems.toList
                in
                Expect.equal atEnd result
        ]


insertWithFocusBefore : Test
insertWithFocusBefore =
    let
        beforeRef =
            termRefFromStr "#b"

        toInsert =
            term

        expected =
            [ Loading (termRefFromStr "#a")
            , Loading termRef
            , Loading (termRefFromStr "#b")
            , Loading (termRefFromStr "#focus")
            , Loading (termRefFromStr "#c")
            , Loading (termRefFromStr "#d")
            ]

        inserted =
            WorkspaceItems.insertWithFocusBefore workspaceItems beforeRef toInsert

        currentFocusedRef =
            getFocusedRef inserted
    in
    describe "WorkspaceItems.insertWithFocusBefore"
        [ test "Inserts before the 'before ref'" <|
            \_ ->
                Expect.equal expected (WorkspaceItems.toList inserted)
        , test "When inserted, the new element has focus" <|
            \_ ->
                Expect.true "Has focus" (Maybe.withDefault False <| Maybe.map (\r -> r == reference toInsert) currentFocusedRef)
        , test "When the 'before hash' is not present, insert at the end" <|
            \_ ->
                let
                    atEnd =
                        [ Loading (termRefFromStr "#a")
                        , Loading (termRefFromStr "#b")
                        , Loading (termRefFromStr "#focus")
                        , Loading (termRefFromStr "#c")
                        , Loading (termRefFromStr "#d")
                        , Loading (reference toInsert)
                        ]

                    result =
                        toInsert
                            |> WorkspaceItems.insertWithFocusAfter workspaceItems notFoundRef
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
                        Failure (termRefFromStr "#b") (Http.BadUrl "err")

                    expected =
                        [ Loading (termRefFromStr "#a")
                        , newItem
                        , Loading (termRefFromStr "#focus")
                        , Loading (termRefFromStr "#c")
                        , Loading (termRefFromStr "#d")
                        ]

                    result =
                        newItem
                            |> WorkspaceItems.replace workspaceItems (termRefFromStr "#b")
                            |> WorkspaceItems.toList
                in
                Expect.equal expected result
        , test "Can replace element in focus" <|
            \_ ->
                let
                    newItem =
                        Failure (termRefFromStr "#focus") (Http.BadUrl "err")

                    expected =
                        [ Loading (termRefFromStr "#a")
                        , Loading (termRefFromStr "#b")
                        , newItem
                        , Loading (termRefFromStr "#c")
                        , Loading (termRefFromStr "#d")
                        ]

                    result =
                        newItem
                            |> WorkspaceItems.replace workspaceItems (termRefFromStr "#focus")
                            |> WorkspaceItems.toList
                in
                Expect.equal expected result
        , test "Can replace element in 'after' focus" <|
            \_ ->
                let
                    newItem =
                        Failure (termRefFromStr "#d") (Http.BadUrl "err")

                    expected =
                        [ Loading (termRefFromStr "#a")
                        , Loading (termRefFromStr "#b")
                        , Loading (termRefFromStr "#focus")
                        , Loading (termRefFromStr "#c")
                        , newItem
                        ]

                    result =
                        newItem
                            |> WorkspaceItems.replace workspaceItems (termRefFromStr "#d")
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
                        termRefFromStr "#a"

                    result =
                        WorkspaceItems.remove workspaceItems toRemove
                in
                Expect.false "#a is removed" (WorkspaceItems.member result toRemove)
        , test "When the element to remove is focused, remove the element and change focus to right after it" <|
            \_ ->
                let
                    toRemove =
                        termRefFromStr "#focus"

                    expectedNewFocus =
                        termRefFromStr "#c"

                    result =
                        WorkspaceItems.remove workspaceItems toRemove
                in
                Expect.true "#focus is removed and #c has focus" (not (WorkspaceItems.member result toRemove) && WorkspaceItems.isFocused result expectedNewFocus)
        , test "When the element to remove is focused and there are no elements after, remove the element and change focus to right before it" <|
            \_ ->
                let
                    toRemove =
                        termRefFromStr "#focus"

                    expectedNewFocus =
                        termRefFromStr "#b"

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
                Expect.equal (Just "term__#c") result
        , test "keeps focus if no elements after" <|
            \_ ->
                let
                    result =
                        WorkspaceItems.fromItems before focused []
                            |> WorkspaceItems.next
                            |> getFocusedRef
                            |> Maybe.map Reference.toString
                in
                Expect.equal (Just "term__#focus") result
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
                Expect.equal (Just "term__#b") result
        , test "keeps focus if no elements before" <|
            \_ ->
                let
                    result =
                        WorkspaceItems.fromItems [] focused after
                            |> WorkspaceItems.prev
                            |> getFocusedRef
                            |> Maybe.map Reference.toString
                in
                Expect.equal (Just "term__#focus") result
        ]



-- MOVE


moveUp : Test
moveUp =
    describe "WorkspaceItems.moveUp"
        [ test "moves the focused item up one position" <|
            \_ ->
                let
                    result =
                        workspaceItems
                            |> WorkspaceItems.moveUp
                            |> WorkspaceItems.toList

                    expected =
                        [ Loading (termRefFromStr "#a")
                        , Loading (termRefFromStr "#focus")
                        , Loading (termRefFromStr "#b")
                        , Loading (termRefFromStr "#c")
                        , Loading (termRefFromStr "#d")
                        ]
                in
                Expect.equal expected result
        , test "first item is first item" <|
            \_ ->
                let
                    result =
                        WorkspaceItems.fromItems [] focused after
                            |> WorkspaceItems.moveUp
                            |> WorkspaceItems.toList

                    expected =
                        [ Loading (termRefFromStr "#focus")
                        , Loading (termRefFromStr "#c")
                        , Loading (termRefFromStr "#d")
                        ]
                in
                Expect.equal expected result
        ]


moveDown : Test
moveDown =
    describe "WorkspaceItems.moveDown"
        [ test "moves the focused item down one position" <|
            \_ ->
                let
                    result =
                        workspaceItems
                            |> WorkspaceItems.moveDown
                            |> WorkspaceItems.toList

                    expected =
                        [ Loading (termRefFromStr "#a")
                        , Loading (termRefFromStr "#b")
                        , Loading (termRefFromStr "#c")
                        , Loading (termRefFromStr "#focus")
                        , Loading (termRefFromStr "#d")
                        ]
                in
                Expect.equal expected result
        , test "last item is last item" <|
            \_ ->
                let
                    result =
                        WorkspaceItems.fromItems before focused []
                            |> WorkspaceItems.moveDown
                            |> WorkspaceItems.toList

                    expected =
                        [ Loading (termRefFromStr "#a")
                        , Loading (termRefFromStr "#b")
                        , Loading (termRefFromStr "#focus")
                        ]
                in
                Expect.equal expected result
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
                            |> WorkspaceItems.map (\i -> Failure (reference i) (Http.BadUrl "err"))
                            |> WorkspaceItems.toList

                    expected =
                        [ Failure (termRefFromStr "#a") (Http.BadUrl "err")
                        , Failure (termRefFromStr "#b") (Http.BadUrl "err")
                        , Failure (termRefFromStr "#focus") (Http.BadUrl "err")
                        , Failure (termRefFromStr "#c") (Http.BadUrl "err")
                        , Failure (termRefFromStr "#d") (Http.BadUrl "err")
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
                        [ "term__#a", "term__#b", "term__#focus", "term__#c", "term__#d" ]
                in
                Expect.equal expected result
        ]



-- TEST HELPERS


termRefFromStr : String -> Reference
termRefFromStr str =
    Reference.fromString TermReference str


termRef : Reference
termRef =
    TermReference hashQualified


notFoundRef : Reference
notFoundRef =
    termRefFromStr "#notfound"


hashQualified : HashQualified
hashQualified =
    HashQualified.fromString "#testhash"


term : WorkspaceItem
term =
    Loading termRef


before : List WorkspaceItem
before =
    [ Loading (termRefFromStr "#a")
    , Loading (termRefFromStr "#b")
    ]


focused : WorkspaceItem
focused =
    Loading (termRefFromStr "#focus")


after : List WorkspaceItem
after =
    [ Loading (termRefFromStr "#c")
    , Loading (termRefFromStr "#d")
    ]


workspaceItems : WorkspaceItems
workspaceItems =
    WorkspaceItems.fromItems before focused after


getFocusedRef : WorkspaceItems -> Maybe Reference
getFocusedRef =
    WorkspaceItems.focus >> Maybe.map reference
