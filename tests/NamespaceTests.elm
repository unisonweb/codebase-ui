module NamespaceTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Nonempty
import Namespace exposing (..)
import Test exposing (..)


pathFromString : Test
pathFromString =
    describe "Namespace.pathFromString"
        [ test "Creates a Path from a string" <|
            \_ ->
                let
                    path =
                        Path (List.Nonempty.Nonempty "a" [ "b", "c" ])
                in
                Expect.equal path (Namespace.pathFromString "a.b.c")
        , describe "With an empty raw path"
            [ test "Creates a root Path from \"\"" <|
                \_ ->
                    let
                        rootPath =
                            Path (List.Nonempty.fromElement ".")
                    in
                    Expect.equal rootPath (Namespace.pathFromString "")
            , test "Creates a root Path from \" \"" <|
                \_ ->
                    let
                        rootPath =
                            Path (List.Nonempty.fromElement ".")
                    in
                    Expect.equal rootPath (Namespace.pathFromString " ")
            , test "Creates a root Path from \".\"" <|
                \_ ->
                    let
                        rootPath =
                            Path (List.Nonempty.fromElement ".")
                    in
                    Expect.equal rootPath (Namespace.pathFromString ".")
            ]
        ]


name : Test
name =
    describe "Namespace.name"
        [ test "Extracts the last portion of a Path" <|
            \_ ->
                Expect.equal "List" (Namespace.name (Path (List.Nonempty.Nonempty "base" [ "List" ])))
        ]
