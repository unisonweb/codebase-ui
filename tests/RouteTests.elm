module RouteTests exposing (..)

import Definition.Reference as Reference exposing (Reference(..))
import Expect
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified as HQ
import Perspective exposing (CodebasePerspectiveParam(..), PerspectiveParams(..))
import Route
import Test exposing (..)
import Url exposing (Url)


perspectiveRoute : Test
perspectiveRoute =
    describe "Route.fromUrl : perspective route"
        [ test "Matches root to relative codease perspective" <|
            \_ ->
                let
                    url =
                        mkUrl "/"

                    expected =
                        Route.Perspective (ByCodebase Relative)
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative perspective" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest"

                    expected =
                        Route.Perspective (ByCodebase Relative)
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative perspective with namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List"

                    expected =
                        Route.Perspective (ByNamespace Relative (fqn "base.List"))
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase absolute perspective" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Route.Perspective (ByCodebase (Absolute h)))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute perspective with namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/namespaces/base/List"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Route.Perspective (ByNamespace (Absolute h) (fqn "base.List")))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a namespace with special characters" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;./%2F/docs"

                    expected =
                        Route.Perspective (ByNamespace Relative (segments [ "base", "List", ".", "/", "docs" ]))
                in
                Expect.equal expected (Route.fromUrl "" url)
        ]


definitionRoute : Test
definitionRoute =
    describe "Route.fromUrl : definition route"
        [ test "Matches a codebase relative and relative definition" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/terms/base/List/map"

                    expected =
                        Route.Definition (ByCodebase Relative) (Reference.fromString TermReference "base.List.map")
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative and relative definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;/terms/map"

                    expected =
                        Route.Definition (ByNamespace Relative (fqn "base.List")) (Reference.fromString TermReference "map")
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative and absolute definition" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/terms/@definitionhash"

                    expected =
                        Route.Definition (ByCodebase Relative) (Reference.fromString TermReference "#definitionhash")
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative and absolute definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;/terms/@definitionhash"

                    expected =
                        Route.Definition (ByNamespace Relative (fqn "base.List")) (Reference.fromString TermReference "#definitionhash")
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase absolute and relative definition " <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/terms/base/List/map"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Route.Definition (ByCodebase (Absolute h)) (Reference.fromString TermReference "base.List.map"))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute and relative definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/namespaces/base/List/;/terms/map"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Route.Definition (ByNamespace (Absolute h) (fqn "base.List")) (Reference.fromString TermReference "map"))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute and absolute definition " <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/terms/@definitionhash"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Route.Definition (ByCodebase (Absolute h)) (Reference.fromString TermReference "#definitionhash"))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute and absolute definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/namespaces/base/List/;/terms/map"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Route.Definition (ByNamespace (Absolute h) (fqn "base.List")) (Reference.fromString TermReference "map"))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a namespace and definition with special characters" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;./%2F/docs/;/terms/docs/about/;./and/%2F/doc"

                    expected =
                        Route.Definition
                            (ByNamespace Relative (segments [ "base", "List", ".", "/", "docs" ]))
                            (TermReference (HQ.NameOnly (segments [ "docs", "about", ".", "and", "/", "doc" ])))
                in
                Expect.equal expected (Route.fromUrl "" url)
        ]


fromUrlBasePath : Test
fromUrlBasePath =
    describe "Route.fromUrl : basePath"
        [ test "Matches with a basePath prefix" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/terms/@abc123"

                    basePath =
                        "/some-token/ui/"

                    expected =
                        Route.Definition (ByCodebase Relative) (Reference.fromString TermReference "#abc123")
                in
                Expect.equal expected (Route.fromUrl basePath url)
        , test "Matches with a root basePath prefix" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/terms/@abc123"

                    basePath =
                        "/"

                    expected =
                        Route.Definition (ByCodebase Relative) (Reference.fromString TermReference "#abc123")
                in
                Expect.equal expected (Route.fromUrl basePath url)
        ]


segments : List String -> FQN
segments =
    FQN.fromList


fqn : String -> FQN
fqn =
    FQN.fromString


hash : String -> Maybe Hash
hash =
    Hash.fromUrlString


mkUrl : String -> Url
mkUrl path =
    { protocol = Url.Https
    , host = "unison-lang.org"
    , port_ = Just 443
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
