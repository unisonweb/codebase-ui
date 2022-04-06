module Lib.Api exposing
    ( ApiBasePath(..)
    , ApiRequest
    , EndpointUrl(..)
    , perform
    , toRequest
    , toTask
    , toUrl
    )

import Http
import Json.Decode as Decode
import Task exposing (Task)
import Url.Builder exposing (QueryParameter, absolute)



{-

   Api
   ===

   Various helpers and types to deal with constructing, building up, and passing
   around Api requests

-}


{-| An EndpointUrl represents a level above a Url String. It includes paths and
query parameters in a structured way such that the structure can be built up
over several steps.
-}
type EndpointUrl
    = EndpointUrl (List String) (List QueryParameter)


{-| A base path to indicate the base of the URL that the HTTP API exists on
ex: ApiBasePath ["api"]
-}
type ApiBasePath
    = ApiBasePath (List String)


toUrl : ApiBasePath -> EndpointUrl -> String
toUrl (ApiBasePath basePath) (EndpointUrl paths queryParams) =
    absolute (basePath ++ paths) queryParams



-- REQUEST --------------------------------------------------------------------


type alias HttpResult a =
    Result Http.Error a


{-| Combines an EndpointUrl with a Decoder and a HttpResult to Msg function.
Required to perform a call to the API.
-}
type ApiRequest a msg
    = ApiRequest EndpointUrl (Decode.Decoder a) (HttpResult a -> msg)


toRequest : Decode.Decoder a -> (HttpResult a -> msg) -> EndpointUrl -> ApiRequest a msg
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
toTask : ApiBasePath -> Decode.Decoder a -> EndpointUrl -> Task Http.Error a
toTask basePath decoder endpoint =
    Http.task
        { method = "GET"
        , headers = []
        , url = toUrl basePath endpoint
        , body = Http.emptyBody
        , resolver = Http.stringResolver (httpJsonBodyResolver decoder)
        , timeout = Nothing
        }


httpJsonBodyResolver : Decode.Decoder a -> Http.Response String -> HttpResult a
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
