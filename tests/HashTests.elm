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


toShortString : Test
toShortString =
    describe "Hash.toShortString"
        [ test "Returns a short version of the hash" <|
            \_ ->
                let
                    result =
                        "#abc123def456"
                            |> Hash.fromString
                            |> Maybe.map Hash.toShortString
                            |> Maybe.withDefault "fail"
                in
                Expect.equal "#abc123de" result
        , test "doesn't shorten for builtins" <|
            \_ ->
                let
                    result =
                        "##IO.socketSend.impl"
                            |> Hash.fromString
                            |> Maybe.map Hash.toShortString
                            |> Maybe.withDefault "fail"
                in
                Expect.equal "##IO.socketSend.impl" result
        ]


stripHashPrefix : Test
stripHashPrefix =
    describe "Hash.stripHashPrefix"
        [ test "removes the prefix of the hash" <|
            \_ ->
                let
                    result =
                        Hash.stripHashPrefix "#abc123def456"
                in
                Expect.equal "abc123def456" result
        , test "removes both hash prefixes for builtins" <|
            \_ ->
                let
                    result =
                        Hash.stripHashPrefix "##IO.socketSend.impl"
                in
                Expect.equal "IO.socketSend.impl" result
        , test "ignores non hashes" <|
            \_ ->
                let
                    result =
                        Hash.stripHashPrefix "thisis#not##ahash"
                in
                Expect.equal "thisis#not##ahash" result
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
