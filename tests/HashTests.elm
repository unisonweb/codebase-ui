module HashTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Hash exposing (..)
import Test exposing (..)


equals : Test
equals =
    describe "Hash.equals"
        [ test "Returns True when equal" <|
            \_ ->
                Expect.true
                    "Expected Hash \"foo\" and Hash \"foo\" to be equal"
                    (Hash.equals (Hash "foo") (Hash "foo"))
        , test "Returns False when not equal" <|
            \_ ->
                Expect.false
                    "Expected Hash \"foo\" and Hash \"bar\" to *not* be equal"
                    (Hash.equals (Hash "foo") (Hash "bar"))
        ]


toString : Test
toString =
    describe "Hash.toString"
        [ test "Extracts the raw hash value" <|
            \_ ->
                let
                    hash =
                        Hash "foo"
                in
                Expect.equal "foo" (Hash.toString hash)
        ]
