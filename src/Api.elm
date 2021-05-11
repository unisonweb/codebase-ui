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

import Http
import Json.Decode as Decode
import Regex
import Syntax
import Url.Builder exposing (QueryParameter, absolute, int, string)


type ApiBasePath
    = ApiBasePath (List String)


type Endpoint
    = Endpoint (List String) (List QueryParameter)


type ApiRequest a msg
    = ApiRequest Endpoint (Decode.Decoder a) (Result Http.Error a -> msg)


{-| TODO: Be more explicit about Root |
-}
list : Maybe String -> Endpoint
list rawFQN =
    rawFQN
        |> Maybe.map (\n -> [ string "namespace" n ])
        |> Maybe.withDefault []
        |> Endpoint [ "list" ]


getDefinition : List String -> Endpoint
getDefinition fqnsOrHashes =
    let
        re =
            Maybe.withDefault Regex.never (Regex.fromString "#[d|a|](\\d+)$")

        stripConstructorPositionFromHash =
            Regex.replace re (always "")
    in
    fqnsOrHashes
        |> List.map stripConstructorPositionFromHash
        |> List.map (string "names")
        |> Endpoint [ "getDefinition" ]


find : Int -> Syntax.Width -> String -> Endpoint
find limit (Syntax.Width sourceWidth) query =
    Endpoint
        [ "find" ]
        [ int "limit" limit
        , int "renderWidth" sourceWidth
        , string "query" query
        ]


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



-- ERROR


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
