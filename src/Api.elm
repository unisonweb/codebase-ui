module Api exposing
    ( ApiBasePath(..)
    , ApiRequest
    , errorToString
    , find
    , getDefinition
    , list
    , perform
    , toRequest
    , toUrl
    )

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Http
import Json.Decode as Decode
import Perspective
    exposing
        ( CodebasePerspectiveParam(..)
        , Perspective(..)
        , PerspectiveParams(..)
        )
import Regex
import Syntax
import Url.Builder exposing (QueryParameter, absolute, int, string)



-- ENDPOINT -------------------------------------------------------------------


type Endpoint
    = Endpoint (List String) (List QueryParameter)


list : PerspectiveParams -> Maybe String -> Endpoint
list perspectiveParams fqnOrHash =
    let
        namespace =
            Maybe.withDefault "." fqnOrHash
    in
    Endpoint [ "list" ] (string "namespace" namespace :: perspectiveParamsToQueryParams perspectiveParams)


getDefinition : Perspective -> List String -> Endpoint
getDefinition perspective fqnsOrHashes =
    let
        re =
            Maybe.withDefault Regex.never (Regex.fromString "#[d|a|](\\d+)$")

        stripConstructorPositionFromHash =
            Regex.replace re (always "")
    in
    fqnsOrHashes
        |> List.map stripConstructorPositionFromHash
        |> List.map (string "names")
        |> (\names -> Endpoint [ "getDefinition" ] (names ++ perspectiveToQueryParams perspective))


find : Perspective -> Int -> Syntax.Width -> String -> Endpoint
find perspective limit (Syntax.Width sourceWidth) query =
    Endpoint
        [ "find" ]
        ([ int "limit" limit
         , int "renderWidth" sourceWidth
         , string "query" query
         ]
            ++ perspectiveToQueryParams perspective
        )



-- REQUEST --------------------------------------------------------------------


type ApiBasePath
    = ApiBasePath (List String)


type ApiRequest a msg
    = ApiRequest Endpoint (Decode.Decoder a) (Result Http.Error a -> msg)


toUrl : ApiBasePath -> Endpoint -> String
toUrl (ApiBasePath basePath) (Endpoint paths queryParams) =
    absolute (basePath ++ paths) queryParams


toRequest : Decode.Decoder a -> (Result Http.Error a -> msg) -> Endpoint -> ApiRequest a msg
toRequest decoder toMsg endpoint =
    ApiRequest endpoint decoder toMsg


perform : ApiBasePath -> ApiRequest a msg -> Cmd msg
perform basePath (ApiRequest endpoint decoder toMsg) =
    Http.get
        { url = toUrl basePath endpoint
        , expect = Http.expectJson toMsg decoder
        }



-- ERROR ----------------------------------------------------------------------


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody text ->
            "Unexpected response from api: " ++ text

        Http.BadUrl url ->
            "Malformed url: " ++ url



-- QUERY PARAMS ---------------------------------------------------------------


perspectiveToQueryParams : Perspective -> List QueryParameter
perspectiveToQueryParams perspective =
    case perspective of
        Codebase h ->
            [ rootBranch h ]

        Namespace d ->
            [ rootBranch d.codebaseHash, string "relativeTo" (FQN.toString d.fqn) ]


perspectiveParamsToQueryParams : PerspectiveParams -> List QueryParameter
perspectiveParamsToQueryParams perspectiveParams =
    case perspectiveParams of
        ByCodebase Relative ->
            []

        ByCodebase (Absolute h) ->
            [ rootBranch h ]

        ByNamespace Relative fqn ->
            [ relativeTo fqn ]

        ByNamespace (Absolute h) fqn ->
            [ rootBranch h, relativeTo fqn ]


rootBranch : Hash -> QueryParameter
rootBranch hash =
    string "rootBranch" (hash |> Hash.toString)


relativeTo : FQN -> QueryParameter
relativeTo fqn =
    string "relativeTo" (fqn |> FQN.toString)
