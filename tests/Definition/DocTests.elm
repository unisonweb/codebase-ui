module Definition.DocTests exposing (..)

import Definition.Doc as Doc exposing (Doc(..))
import Expect
import Test exposing (..)
import TreePath


mergeWords : Test
mergeWords =
    describe "Doc.mergeWords"
        [ test "merges adjacent Word elements with a separator" <|
            \_ ->
                let
                    before =
                        [ Word "Hello", Word "World", Blankline, Word "After", Word "non", Word "word" ]

                    expected =
                        [ Word "Hello World", Blankline, Word "After non word" ]
                in
                Expect.equal expected (Doc.mergeWords " " before)
        ]


isDocFoldToggled : Test
isDocFoldToggled =
    describe "Doc.isDocFoldToggled"
        [ test "returns True if the doc is toggled" <|
            \_ ->
                let
                    toggles =
                        Doc.toggleFold Doc.emptyDocFoldToggles id
                in
                Expect.true "doc is toggled" (Doc.isDocFoldToggled toggles id)
        , test "returns False if the doc is not toggled" <|
            \_ ->
                let
                    toggles =
                        Doc.emptyDocFoldToggles
                in
                Expect.false "doc is not toggled" (Doc.isDocFoldToggled toggles id)
        ]


toString : Test
toString =
    describe "Doc.toString"
        [ test "merges docs down to a string with a separator" <|
            \_ ->
                let
                    before =
                        Span [ Word "Hello", Word "World", Blankline, Word "After", Word "non", Word "word" ]

                    expected =
                        "Hello World After non word"
                in
                Expect.equal expected (Doc.toString " " before)
        ]


toggleFold : Test
toggleFold =
    describe "Doc.toggleFold"
        [ test "Adds a toggle if not present" <|
            \_ ->
                let
                    toggles =
                        Doc.toggleFold Doc.emptyDocFoldToggles id
                in
                Expect.true "doc was added" (Doc.isDocFoldToggled toggles id)
        , test "Removes a toggle if present" <|
            \_ ->
                let
                    toggles =
                        Doc.toggleFold Doc.emptyDocFoldToggles id

                    without =
                        Doc.toggleFold toggles id
                in
                Expect.false "doc was removed" (Doc.isDocFoldToggled without id)
        ]



-- Helpers


id : Doc.FoldId
id =
    Doc.FoldId [ TreePath.VariantIndex 0, TreePath.ListIndex 3 ]
