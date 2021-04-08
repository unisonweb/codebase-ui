module HashTests exposing (..)

import Expect
import Hash
import Test exposing (..)


equals : Test
equals =
    describe "Hash.equals"
        [ test "Returns True when equal" <|
            \_ ->
                Expect.true
                    "Expected Hash \"#foo\" and Hash \"#foo\" to be equal"
                    (Maybe.withDefault False (Maybe.map2 Hash.equals (Hash.fromString "#foo") (Hash.fromString "#foo")))
        , test "Returns False when not equal" <|
            \_ ->
                Expect.false
                    "Expected Hash \"#foo\" and Hash \"#bar\" to *not* be equal"
                    (Maybe.withDefault False (Maybe.map2 Hash.equals (Hash.fromString "#foo") (Hash.fromString "#bar")))
        ]


toString : Test
toString =
    describe "Hash.toString"
        [ test "Extracts the raw hash value" <|
            \_ ->
                let
                    result =
                        "#foo"
                            |> Hash.fromString
                            |> Maybe.map Hash.toString
                            |> Maybe.withDefault "fail"
                in
                Expect.equal "#foo" result
        ]


toUrlString : Test
toUrlString =
    describe "Hash.toUrlString"
        [ test "Extracts the raw hash value in a URL format" <|
            \_ ->
                let
                    result =
                        "#foo"
                            |> Hash.fromString
                            |> Maybe.map Hash.toUrlString
                            |> Maybe.withDefault "fail"
                in
                Expect.equal "@foo" result
        ]


fromString : Test
fromString =
    describe "Hash.fromString"
        [ test "Creates a Hash with a valid prefixed raw hash" <|
            \_ ->
                let
                    hash =
                        Hash.fromString "#foo"
                in
                Expect.equal (Just "#foo") (Maybe.map Hash.toString hash)
        , test "Fails to create a hash with an incorrect prefix" <|
            \_ ->
                let
                    hash =
                        Hash.fromString "$foo"
                in
                Expect.equal Nothing (Maybe.map Hash.toString hash)
        , test "Fails to create a hash with an @ symbol prefix" <|
            \_ ->
                let
                    hash =
                        Hash.fromString "@foo"
                in
                Expect.equal Nothing (Maybe.map Hash.toString hash)
        , test "Fails to create a hash with no symbol prefix" <|
            \_ ->
                let
                    hash =
                        Hash.fromString "foo"
                in
                Expect.equal Nothing (Maybe.map Hash.toString hash)
        ]


fromUrlString : Test
fromUrlString =
    describe "Hash.fromUrlString"
        [ test "Creates a Hash with a valid URL prefixed raw hash" <|
            \_ ->
                let
                    hash =
                        Hash.fromUrlString "@foo"
                in
                Expect.equal (Just "#foo") (Maybe.map Hash.toString hash)
        , test "Fails to create a hash with an incorrect prefix" <|
            \_ ->
                let
                    hash =
                        Hash.fromUrlString "$foo"
                in
                Expect.equal Nothing (Maybe.map Hash.toString hash)
        , test "Fails to create a hash with an # symbol prefix" <|
            \_ ->
                let
                    hash =
                        Hash.fromUrlString "#foo"
                in
                Expect.equal Nothing (Maybe.map Hash.toString hash)
        , test "Fails to create a hash with no symbol prefix" <|
            \_ ->
                let
                    hash =
                        Hash.fromUrlString "foo"
                in
                Expect.equal Nothing (Maybe.map Hash.toString hash)
        ]


isRawHash : Test
isRawHash =
    describe "Hash.isRawHash"
        [ test "True for strings prefixed with #" <|
            \_ -> Expect.true "# is a raw hash" (Hash.isRawHash "#foo")
        , test "True for strings prefixed with @" <|
            \_ -> Expect.true "@ is a raw hash" (Hash.isRawHash "@foo")
        , test "False for non prefixed strings" <|
            \_ -> Expect.false "needs prefix" (Hash.isRawHash "foo")
        ]
