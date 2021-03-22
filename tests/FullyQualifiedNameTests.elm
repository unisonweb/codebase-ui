module FullyQualifiedNameTests exposing (..)

import Expect
import FullyQualifiedName as FQN exposing (..)
import Test exposing (..)


fromString : Test
fromString =
    describe "FullyQualifiedName.fromString"
        [ test "Creates an FQN from a string" <|
            \_ ->
                Expect.equal "a.b.c" (FQN.toString (FQN.fromString "a.b.c"))
        , describe "Root"
            [ test "Creates a root FQN from \"\"" <|
                \_ ->
                    Expect.equal "." (FQN.toString (FQN.fromString ""))
            , test "Creates a root FQN from \" \"" <|
                \_ ->
                    Expect.equal "." (FQN.toString (FQN.fromString " "))
            , test "Creates a root FQN from \".\"" <|
                \_ ->
                    Expect.equal "." (FQN.toString (FQN.fromString "."))
            ]
        ]


toString : Test
toString =
    describe "FullyQualifiedName.toString"
        [ test "serializes the FQN" <|
            \_ ->
                Expect.equal "foo.bar" (FQN.toString (FQN.fromString "foo.bar"))
        , test "includes root dot when an absolute fqn" <|
            \_ ->
                Expect.equal ".foo.bar" (FQN.toString (FQN.fromString ".foo.bar"))
        ]


fromParent : Test
fromParent =
    describe "FullyQualifiedName.fromParent"
        [ test "Combines a name and parent FQN" <|
            \_ ->
                Expect.equal "foo.bar.baz" (FQN.toString (FQN.fromParent (FQN.fromString "foo.bar") "baz"))
        ]


unqualifiedName : Test
unqualifiedName =
    describe "FullyQualifiedName.unqualifiedName"
        [ test "Extracts the last portion of a FQN" <|
            \_ ->
                Expect.equal "List" (FQN.unqualifiedName (FQN.fromString "base.List"))
        ]
