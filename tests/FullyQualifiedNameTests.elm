module FullyQualifiedNameTests exposing (..)

import Expect exposing (Expectation)
import FullyQualifiedName exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Nonempty
import Test exposing (..)


fqn : Test
fqn =
    describe "FullyQualifiedName.fqn"
        [ test "Creates an FQN from a string" <|
            \_ ->
                let
                    path =
                        FQN (List.Nonempty.Nonempty "a" [ "b", "c" ])
                in
                Expect.equal path (FullyQualifiedName.fqn "a.b.c")
        , describe "With an empty raw path"
            [ test "Creates a root FQN from \"\"" <|
                \_ ->
                    let
                        rootFQN =
                            FQN (List.Nonempty.fromElement ".")
                    in
                    Expect.equal rootFQN (FullyQualifiedName.fqn "")
            , test "Creates a root FQN from \" \"" <|
                \_ ->
                    let
                        rootFQN =
                            FQN (List.Nonempty.fromElement ".")
                    in
                    Expect.equal rootFQN (FullyQualifiedName.fqn " ")
            , test "Creates a root FQN from \".\"" <|
                \_ ->
                    let
                        rootFQN =
                            FQN (List.Nonempty.fromElement ".")
                    in
                    Expect.equal rootFQN (FullyQualifiedName.fqn ".")
            ]
        ]


unqualifiedName : Test
unqualifiedName =
    describe "FullyQualifiedName.unqualifiedName"
        [ test "Extracts the last portion of a FQN" <|
            \_ ->
                Expect.equal "List" (FullyQualifiedName.unqualifiedName (FQN (List.Nonempty.Nonempty "base" [ "List" ])))
        ]
