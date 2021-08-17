module FullyQualifiedNameTests exposing (..)

import Expect
import FullyQualifiedName as FQN exposing (..)
import List.Nonempty as NEL
import Test exposing (..)


fromString : Test
fromString =
    describe "FullyQualifiedName.fromString"
        [ test "Creates an FQN from a string" <|
            \_ ->
                Expect.equal [ "a", "b", "c" ] (segments (FQN.fromString "a.b.c"))
        , test "Creates an FQN from a string where a segment includes a dot (like the composition operatory)" <|
            \_ ->
                Expect.equal [ "base", "." ] (segments (FQN.fromString "base.."))
        , describe "Root"
            [ test "Creates a root FQN from \"\"" <|
                \_ ->
                    Expect.equal [ "." ] (segments (FQN.fromString ""))
            , test "Creates a root FQN from \" \"" <|
                \_ ->
                    Expect.equal [ "." ] (segments (FQN.fromString " "))
            , test "Creates a root FQN from \".\"" <|
                \_ ->
                    Expect.equal [ "." ] (segments (FQN.fromString "."))
            ]
        ]


fromUrlString : Test
fromUrlString =
    describe "FullyQualifiedName.fromUrlString"
        [ test "Creates an FQN from a URL string (segments separate by /)" <|
            \_ ->
                Expect.equal [ "a", "b", "c" ] (segments (FQN.fromUrlString "a/b/c"))
        , test "Supports . in segments (compose)" <|
            \_ ->
                Expect.equal [ "a", "b", "." ] (segments (FQN.fromUrlString "a/b/."))
        , test "Supports special characters n segments" <|
            \_ ->
                let
                    results =
                        [ segments (FQN.fromUrlString "a/b/+")
                        , segments (FQN.fromUrlString "a/b/*")
                        , segments (FQN.fromUrlString "a/b/%2F") -- /
                        , segments (FQN.fromUrlString "a/b/%25") -- %
                        , segments (FQN.fromUrlString "a/b/!")
                        , segments (FQN.fromUrlString "a/b/-")
                        , segments (FQN.fromUrlString "a/b/==")
                        ]

                    expects =
                        [ [ "a", "b", "+" ]
                        , [ "a", "b", "*" ]
                        , [ "a", "b", "/" ]
                        , [ "a", "b", "%" ]
                        , [ "a", "b", "!" ]
                        , [ "a", "b", "-" ]
                        , [ "a", "b", "==" ]
                        ]
                in
                Expect.equal expects results
        , describe "Root"
            [ test "Creates a root FQN from \"\"" <|
                \_ ->
                    Expect.equal [ "." ] (segments (FQN.fromUrlString ""))
            , test "Creates a root FQN from \" \"" <|
                \_ ->
                    Expect.equal [ "." ] (segments (FQN.fromUrlString " "))
            , test "Creates a root FQN from \"/\"" <|
                \_ ->
                    Expect.equal [ "." ] (segments (FQN.fromUrlString "/"))
            ]
        ]


toString : Test
toString =
    describe "FullyQualifiedName.toString with segments separate by ."
        [ test "serializes the FQN" <|
            \_ ->
                Expect.equal "foo.bar" (FQN.toString (FQN.fromString "foo.bar"))
        , test "it supports . as term names (compose)" <|
            \_ ->
                Expect.equal "foo.bar.." (FQN.toString (FQN.fromString "foo.bar.."))
        ]


toUrlString : Test
toUrlString =
    describe "FullyQualifiedName.toUrlString"
        [ test "serializes the FQN with segments separate by /" <|
            \_ ->
                Expect.equal "foo/bar" (FQN.toUrlString (FQN.fromString "foo.bar"))
        , test "URL encodes / (divide) segments" <|
            \_ ->
                Expect.equal "foo/bar/%2F/doc" (FQN.toUrlString (FQN.fromString "foo.bar./.doc"))
        , test "URL encodes % segments" <|
            \_ ->
                Expect.equal "foo/bar/%25/doc" (FQN.toUrlString (FQN.fromString "foo.bar.%.doc"))
        , test "URL encodes . segments with a ; prefix" <|
            \_ ->
                Expect.equal "foo/bar/;./doc" (FQN.toUrlString (FQN.fromString "foo.bar...doc"))
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


namepsace : Test
namepsace =
    describe "FullyQualifiedName.namepsace"
        [ test "removes qualified name" <|
            \_ ->
                let
                    fqn =
                        FQN.fromString "base.List.map"
                in
                Expect.equal (Just (FQN.fromString "base.List")) (FQN.namespace fqn)
        , test "with an FQN of only 1 segment, it returns Nothing" <|
            \_ ->
                let
                    fqn =
                        FQN.fromString "map"
                in
                Expect.equal Nothing (FQN.namespace fqn)
        ]



-- HELPERS


segments : FQN -> List String
segments =
    FQN.segments >> NEL.toList
