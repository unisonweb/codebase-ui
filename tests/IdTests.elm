module IdTests exposing (..)

import Expect
import Id
import Test exposing (..)


equals : Test
equals =
    describe "Id.equals"
        [ test "Returns True when equal" <|
            \_ ->
                Expect.true
                    "Expected Id \"abc\" and Id \"abc\" to be equal"
                    (Id.equals (Id.fromString "abc") (Id.fromString "abc"))
        , test "Returns False when not equal" <|
            \_ ->
                Expect.false
                    "Expected Id \"abc\" and Id \"def\" to *not* be equal"
                    (Id.equals (Id.fromString "abc") (Id.fromString "def"))
        ]


fromAndToString : Test
fromAndToString =
    describe "Id fromString and toString"
        [ test "Extracts the raw id value" <|
            \_ ->
                let
                    result =
                        "abc"
                            |> Id.fromString
                            |> Id.toString
                in
                Expect.equal "abc" result
        ]
