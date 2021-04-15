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


fromUrlString : Test
fromUrlString =
    describe "FullyQualifiedName.fromUrlString"
        [ test "Creates an FQN from a URL string (segments separate by /)" <|
            \_ ->
                Expect.equal "a.b.c" (FQN.toString (FQN.fromUrlString "a/b/c"))
        , describe "Root"
            [ test "Creates a root FQN from \"\"" <|
                \_ ->
                    Expect.equal "." (FQN.toString (FQN.fromUrlString ""))
            , test "Creates a root FQN from \" \"" <|
                \_ ->
                    Expect.equal "." (FQN.toString (FQN.fromUrlString " "))
            , test "Creates a root FQN from \"/\"" <|
                \_ ->
                    Expect.equal "." (FQN.toString (FQN.fromUrlString "/"))
            ]
        ]


toString : Test
toString =
    describe "FullyQualifiedName.toString with segments separate by ."
        [ test "serializes the FQN" <|
            \_ ->
                Expect.equal "foo.bar" (FQN.toString (FQN.fromString "foo.bar"))
        , test "includes root dot when an absolute fqn" <|
            \_ ->
                Expect.equal ".foo.bar" (FQN.toString (FQN.fromString ".foo.bar"))
        ]


toUrlString : Test
toUrlString =
    describe "FullyQualifiedName.toUrlString"
        [ test "serializes the FQN with segments separate by /" <|
            \_ ->
                Expect.equal "foo/bar" (FQN.toUrlString (FQN.fromString "foo.bar"))
        , test "includes root dot when an absolute fqn" <|
            \_ ->
                Expect.equal "/foo/bar" (FQN.toUrlString (FQN.fromString ".foo.bar"))
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


isSuffixOf : Test
isSuffixOf =
    describe "FullyQualifiedName.isSuffixOf"
        [ test "Returns True when an FQN ends in the provided suffix" <|
            \_ ->
                let
                    suffix =
                        "List.map"

                    fqn =
                        FQN.fromString "base.List.map"
                in
                Expect.true "is correctly a suffix of" (FQN.isSuffixOf suffix fqn)
        , test "Returns False when an FQN does not end in the provided suffix" <|
            \_ ->
                let
                    suffix =
                        "List.foldl"

                    fqn =
                        FQN.fromString "base.List.map"
                in
                Expect.false "is correctly *not* a suffix of" (FQN.isSuffixOf suffix fqn)
        ]


namespaceOf : Test
namespaceOf =
    describe "FullyQualifiedName.namespaceOf"
        [ test "With an FQN including the suffix, it returns the non suffix part" <|
            \_ ->
                let
                    suffix =
                        "List.map"

                    fqn =
                        FQN.fromString "base.List.map"
                in
                Expect.equal (Just "base") (FQN.namespaceOf suffix fqn)
        , test "When the suffix and FQN are exactly the same, it returns Nothing" <|
            \_ ->
                let
                    suffix =
                        "base.List.map"

                    fqn =
                        FQN.fromString "base.List.map"
                in
                Expect.equal Nothing (FQN.namespaceOf suffix fqn)
        , test "When the suffix is not included at all in the FQN, it returns Nothing" <|
            \_ ->
                let
                    suffix =
                        "List.map.foldl"

                    fqn =
                        FQN.fromString "base.List.map"
                in
                Expect.equal Nothing (FQN.namespaceOf suffix fqn)
        , test "When the suffix is included more than once, only the last match of the FQN is removed" <|
            \_ ->
                let
                    suffix =
                        "Map"

                    fqn =
                        FQN.fromString "base.Map.Map"
                in
                Expect.equal (Just "base.Map") (FQN.namespaceOf suffix fqn)
        ]
