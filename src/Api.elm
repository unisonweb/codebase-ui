module Api exposing
    ( ApiBasePath(..)
    , ApiRequest
    , codebaseHash
    , errorToString
    , find
    , getDefinition
    , list
    , namespace
    , perform
    , toRequest
    , toUrl
    )

import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Http
import Json.Decode as Decode
import Perspective exposing (Perspective(..))
import Regex
import Syntax
import Url.Builder exposing (QueryParameter, absolute, int, string)



-- ENDPOINT -------------------------------------------------------------------


type Endpoint
    = Endpoint (List String) (List QueryParameter)


codebaseHash : Endpoint
codebaseHash =
    Endpoint [ "list" ] [ string "namespace" "." ]


list : Perspective -> Maybe String -> Endpoint
list perspective fqnOrHash =
    let
        namespace_ =
            Maybe.withDefault "." fqnOrHash
    in
    Endpoint [ "list" ] (string "namespace" namespace_ :: perspectiveToQueryParams perspective)


namespace : Perspective -> FQN -> Endpoint
namespace perspective fqn =
    let
        queryParams =
            [ rootBranch (Perspective.codebaseHash perspective) ]
    in
    Endpoint [ "namespaces", FQN.toString fqn ] queryParams


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


find : Perspective -> Maybe FQN -> Int -> Syntax.Width -> String -> Endpoint
find perspective withinFqn limit (Syntax.Width sourceWidth) query =
    let
        params =
            case withinFqn of
                Just fqn ->
                    [ rootBranch (Perspective.codebaseHash perspective), relativeTo fqn ]

                Nothing ->
                    perspectiveToQueryParams perspective
    in
    Endpoint
        [ "find" ]
        ([ int "limit" limit
         , int "renderWidth" sourceWidth
         , string "query" query
         ]
            ++ params
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
            [ rootBranch d.codebaseHash, relativeTo d.fqn ]


rootBranch : Hash -> QueryParameter
rootBranch hash =
    string "rootBranch" (hash |> Hash.toString)


relativeTo : FQN -> QueryParameter
relativeTo fqn =
    string "relativeTo" (fqn |> FQN.toString)
