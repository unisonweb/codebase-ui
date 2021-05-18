module RouteTests exposing (..)

import Definition.Reference as Reference exposing (Reference(..))
import Expect
import RelativeTo
import Route
import Test exposing (..)
import Url exposing (Url)


fromUrl : Test
fromUrl =
    describe "Route.fromUrl"
        [ test "Matches with a basePath prefix" <|
            \_ ->
                let
                    url =
                        mkUrl "/latest/terms/@abc123"

                    basePath =
                        "/some-token/ui/"

                    expected =
                        Route.ByReference RelativeTo.Codebase (Reference.fromString TermReference "#abc123")
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
                        Route.ByReference RelativeTo.Codebase (Reference.fromString TermReference "#abc123")
                in
                Expect.equal expected (Route.fromUrl basePath url)
        ]


mkUrl : String -> Url
mkUrl path =
    { protocol = Url.Https
    , host = "unison-lang.org"
    , port_ = Just 443
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
