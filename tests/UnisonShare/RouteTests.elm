module UnisonShare.RouteTests exposing (..)

import Definition.Reference as Reference exposing (Reference(..))
import Expect
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified as HQ
import Perspective exposing (CodebasePerspectiveParam(..), PerspectiveParams(..))
import Test exposing (..)
import UnisonShare.Route as Route exposing (ProjectRoute(..), Route(..))
import Url exposing (Url)


catalogRoute : Test
catalogRoute =
    describe "Route.fromUrl : catalog route"
        [ test "Matches /catalog to Catalog" <|
            \_ ->
                let
                    url =
                        mkUrl "/catalog"
                in
                Expect.equal Catalog (Route.fromUrl "" url)
        , test "Matches root to Catalog" <|
            \_ ->
                let
                    url =
                        mkUrl "/"
                in
                Expect.equal Catalog (Route.fromUrl "" url)
        ]


projectRootRoute : Test
projectRootRoute =
    describe "Route.fromUrl : project root route"
        [ test "Matches a codebase relative perspective" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest"

                    expected =
                        Project (ByCodebase Relative) ProjectRoot
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative perspective with namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List"

                    expected =
                        Project (ByNamespace Relative (fqn "base.List")) ProjectRoot
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase absolute perspective" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Project (ByCodebase (Absolute h)) ProjectRoot)
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute perspective with namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/namespaces/base/List"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Project (ByNamespace (Absolute h) (fqn "base.List")) ProjectRoot)
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a namespace with special characters" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;./%2F/docs"

                    expected =
                        Route.Project (ByNamespace Relative (segments [ "base", "List", ".", "/", "docs" ])) ProjectRoot
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
                        Project (ByCodebase Relative) (ProjectDefinition (Reference.fromString TermReference "base.List.map"))
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative and relative definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;/terms/map"

                    expected =
                        Project (ByNamespace Relative (fqn "base.List")) (ProjectDefinition (Reference.fromString TermReference "map"))
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative and absolute definition" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/terms/@definitionhash"

                    expected =
                        Project (ByCodebase Relative) (ProjectDefinition (Reference.fromString TermReference "#definitionhash"))
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase relative and absolute definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;/terms/@definitionhash"

                    expected =
                        Project (ByNamespace Relative (fqn "base.List")) (ProjectDefinition (Reference.fromString TermReference "#definitionhash"))
                in
                Expect.equal expected (Route.fromUrl "" url)
        , test "Matches a codebase absolute and relative definition " <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/terms/base/List/map"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Project (ByCodebase (Absolute h)) (ProjectDefinition (Reference.fromString TermReference "base.List.map")))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute and relative definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/namespaces/base/List/;/terms/map"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Project (ByNamespace (Absolute h) (fqn "base.List")) (ProjectDefinition (Reference.fromString TermReference "map")))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute and absolute definition " <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/terms/@definitionhash"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Project (ByCodebase (Absolute h)) (ProjectDefinition (Reference.fromString TermReference "#definitionhash")))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a codebase absolute and absolute definition within a namespace" <|
            \_ ->
                let
                    url =
                        mkUrl "/@codebasehash/namespaces/base/List/;/terms/map"

                    expected =
                        hash "@codebasehash"
                            |> Maybe.map (\h -> Project (ByNamespace (Absolute h) (fqn "base.List")) (ProjectDefinition (Reference.fromString TermReference "map")))
                in
                Expect.equal expected (Just (Route.fromUrl "" url))
        , test "Matches a namespace and definition with special characters" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/namespaces/base/List/;./%2F/docs/;/terms/docs/about/;./and/%2F/doc"

                    expected =
                        Project
                            (ByNamespace Relative (segments [ "base", "List", ".", "/", "docs" ]))
                            (ProjectDefinition
                                (TermReference (HQ.NameOnly (segments [ "docs", "about", ".", "and", "/", "doc" ])))
                            )
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
                        mkUrl "/some-token/ui/latest/terms/@abc123"

                    basePath =
                        "/some-token/ui/"

                    expected =
                        Project (ByCodebase Relative) (ProjectDefinition (Reference.fromString TermReference "#abc123"))
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
                        Project (ByCodebase Relative) (ProjectDefinition (Reference.fromString TermReference "#abc123"))
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
