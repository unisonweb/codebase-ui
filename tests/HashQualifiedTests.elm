module HashQualifiedTests exposing (..)

import Expect
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified
import Test exposing (..)


name : Test
name =
    describe "HashQualified.name"
        [ test "Returns Just name when NameOnly" <|
            \_ ->
                let
                    hq =
                        HashQualified.NameOnly name_
                in
                Expect.equal (Just "test.name") (Maybe.map FQN.toString (HashQualified.name hq))
        , test "Returns Nothing when HashOnly" <|
            \_ ->
                let
                    hq =
                        HashQualified.HashOnly hash_
                in
                Expect.equal Nothing (HashQualified.name hq)
        , test "Returns Just name when HashQualified" <|
            \_ ->
                let
                    hq =
                        HashQualified.HashQualified name_ hash_
                in
                Expect.equal (Just "test.name") (Maybe.map FQN.toString (HashQualified.name hq))
        ]


hash : Test
hash =
    describe "HashQualified.hash"
        [ test "Returns Nothing when NameOnly" <|
            \_ ->
                let
                    hq =
                        HashQualified.NameOnly name_
                in
                Expect.equal Nothing (HashQualified.hash hq)
        , test "Returns Just hash when HashOnly" <|
            \_ ->
                let
                    hq =
                        HashQualified.HashOnly hash_
                in
                Expect.equal (Just "#testhash") (Maybe.map Hash.toString (HashQualified.hash hq))
        , test "Returns Just hash when HashQualified" <|
            \_ ->
                let
                    hq =
                        HashQualified.HashQualified name_ hash_
                in
                Expect.equal (Just "#testhash") (Maybe.map Hash.toString (HashQualified.hash hq))
        ]


fromUrlString : Test
fromUrlString =
    describe "HashQualified.fromUrlString"
        [ test "HashOnly when called with a raw hash" <|
            \_ ->
                let
                    expected =
                        HashQualified.HashOnly hash_
                in
                Expect.equal expected (HashQualified.fromUrlString "@testhash")
        , test "HashQualified when called with a string and name" <|
            \_ ->
                let
                    expected =
                        HashQualified.HashQualified name_ hash_
                in
                Expect.equal expected (HashQualified.fromUrlString "test.name@testhash")
        , test "NameOnly when called with a name" <|
            \_ ->
                let
                    expected =
                        HashQualified.NameOnly name_
                in
                Expect.equal expected (HashQualified.fromUrlString "test.name")
        ]



-- TEST HELPERS


name_ : FQN
name_ =
    FQN.fromString "test.name"


hash_ : Hash
hash_ =
    Hash.fromString "#testhash"
