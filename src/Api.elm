module Api exposing (..)

import Http
import RemoteData exposing (WebData)
import Url.Builder exposing (QueryParameter, absolute)



-- URL HELPERS


serverUrl : List String -> List QueryParameter -> String
serverUrl path queryParams =
    absolute ("api" :: path) queryParams


listUrl : String
listUrl =
    serverUrl [ "list" ] []


definitionUrl : String
definitionUrl =
    serverUrl [ "getDefinition" ] []



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
