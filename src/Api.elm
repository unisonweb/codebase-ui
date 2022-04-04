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
    , projects
    , toRequest
    , toTask
    , toUrl
    )

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.Syntax as Syntax
import Http
import Json.Decode as Decode
import Regex
import Task exposing (Task)
import Url.Builder exposing (QueryParameter, absolute, int, string)



-- ENDPOINT -------------------------------------------------------------------


type Endpoint
    = Endpoint (List String) (List QueryParameter)


toUrl : ApiBasePath -> Endpoint -> String
toUrl (ApiBasePath basePath) (Endpoint paths queryParams) =
    absolute (basePath ++ paths) queryParams


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


projects : Maybe String -> Endpoint
projects owner =
    let
        queryParams =
            case owner of
                Just owner_ ->
                    [ string "owner" owner_ ]

                Nothing ->
                    []
    in
    Endpoint [ "projects" ] queryParams


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


toRequest : Decode.Decoder a -> (Result Http.Error a -> msg) -> Endpoint -> ApiRequest a msg
toRequest decoder toMsg endpoint =
    ApiRequest endpoint decoder toMsg


perform : ApiBasePath -> ApiRequest a msg -> Cmd msg
perform basePath (ApiRequest endpoint decoder toMsg) =
    Http.get
        { url = toUrl basePath endpoint
        , expect = Http.expectJson toMsg decoder
        }



--- TASK ----------------------------------------------------------------------


{-| TODO Perhaps this API should be merged into ApiRequest fully?? |
-}
toTask : ApiBasePath -> Decode.Decoder a -> Endpoint -> Task Http.Error a
toTask basePath decoder endpoint =
    Http.task
        { method = "GET"
        , headers = []
        , url = toUrl basePath endpoint
        , body = Http.emptyBody
        , resolver = Http.stringResolver (httpJsonBodyResolver decoder)
        , timeout = Nothing
        }


httpJsonBodyResolver : Decode.Decoder a -> Http.Response String -> Result Http.Error a
httpJsonBodyResolver decoder resp =
    case resp of
        Http.GoodStatus_ _ s ->
            Decode.decodeString decoder s
                |> Result.mapError (Decode.errorToString >> Http.BadBody)

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ m s ->
            Decode.decodeString decoder s
                -- just trying; if our decoder understands the response body, great
                |> Result.mapError (\_ -> Http.BadStatus m.statusCode)



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
