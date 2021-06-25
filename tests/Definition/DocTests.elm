module Definition.DocTests exposing (..)

import Definition.Doc as Doc exposing (Doc)
import Expect
import Id exposing (Id)
import Test exposing (..)


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


id : Id Doc
id =
    Id.fromString "abcdef"
