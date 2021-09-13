module Finder.SearchOptionsTests exposing (..)

import Expect
import Finder.SearchOptions as SearchOptions exposing (SearchOptions(..), WithinOption(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash
import Perspective exposing (Perspective)
import Test exposing (..)


init : Test
init =
    describe "Finder.SearchOptions.init"
        [ test "with an FQN and the Codebase Perspective it returns the WithinNamespace WithinOption" <|
            \_ ->
                let
                    result =
                        codebasePerspective
                            |> Maybe.map (\p -> SearchOptions.init p (Just namespaceFqn))
                in
                Expect.equal (Just (SearchOptions (WithinNamespace namespaceFqn))) result
        , test "with an FQN and the Namespace Perspective it returns the WithinNamespace WithinOption" <|
            \_ ->
                let
                    result =
                        namespacePerspective
                            |> Maybe.map (\p -> SearchOptions.init p (Just namespaceFqn))
                in
                Expect.equal (Just (SearchOptions (WithinNamespace namespaceFqn))) result
        , test "without an FQN and the Codebase Perspective it returns the AllNamespaces WithinOption" <|
            \_ ->
                let
                    result =
                        codebasePerspective
                            |> Maybe.map (\p -> SearchOptions.init p Nothing)
                in
                Expect.equal (Just (SearchOptions AllNamespaces)) result
        , test "without an FQN and the Namespace Perspective it returns the WithinNamespacePerspective WithinOption" <|
            \_ ->
                let
                    result =
                        namespacePerspective
                            |> Maybe.map (\p -> SearchOptions.init p Nothing)
                in
                Expect.equal (Just (SearchOptions (WithinNamespacePerspective perspectiveFqn))) result
        ]


removeWithin : Test
removeWithin =
    describe "Finder.SearchOptions.removeWithin"
        [ test "when removing AllNamespaces it returns the AllNamespaces WithinOption" <|
            \_ ->
                let
                    initial =
                        SearchOptions AllNamespaces

                    result =
                        codebasePerspective
                            |> Maybe.map (\p -> SearchOptions.removeWithin p initial)
                in
                Expect.equal (Just (SearchOptions AllNamespaces)) result
        , test "when removing WithinNamespacePerspective it returns the AllNamespaces WithinOption" <|
            \_ ->
                let
                    initial =
                        SearchOptions (SearchOptions.WithinNamespacePerspective perspectiveFqn)

                    result =
                        namespacePerspective
                            |> Maybe.map (\p -> SearchOptions.removeWithin p initial)
                in
                Expect.equal (Just (SearchOptions AllNamespaces)) result
        , test "when removing WithinNamespace and Perspective is Codebase, it returns the AllNamespaces WithinOption" <|
            \_ ->
                let
                    initial =
                        SearchOptions (SearchOptions.WithinNamespace namespaceFqn)

                    result =
                        codebasePerspective
                            |> Maybe.map (\p -> SearchOptions.removeWithin p initial)
                in
                Expect.equal (Just (SearchOptions AllNamespaces)) result
        , test "when removing WithinNamespace and Perspective is Namespace, it returns the WithinNamespacePerspective WithinOption" <|
            \_ ->
                let
                    initial =
                        SearchOptions (SearchOptions.WithinNamespace namespaceFqn)

                    result =
                        namespacePerspective
                            |> Maybe.map (\p -> SearchOptions.removeWithin p initial)
                in
                Expect.equal (Just (SearchOptions (WithinNamespacePerspective perspectiveFqn))) result
        ]



-- HELPERS


namespaceFqn : FQN
namespaceFqn =
    FQN.fromString "namespace.FQN"


perspectiveFqn : FQN
perspectiveFqn =
    FQN.fromString "perspective.FQN"


namespacePerspective : Maybe Perspective
namespacePerspective =
    Hash.fromString "#testhash"
        |> Maybe.map (\h -> Perspective.Namespace { codebaseHash = h, fqn = perspectiveFqn })


codebasePerspective : Maybe Perspective
codebasePerspective =
    Hash.fromString "#testhash"
        |> Maybe.map Perspective.Codebase
