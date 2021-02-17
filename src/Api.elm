module Api exposing (..)

import Http
import RemoteData exposing (WebData)
import Url.Builder exposing (QueryParameter, absolute, string)



-- URL HELPERS


serverUrl : List String -> List QueryParameter -> String
serverUrl path queryParams =
    absolute ("api" :: path) queryParams


{-| TODO: Be more explicit about Root |
-}
listUrl : Maybe String -> String
listUrl rawFQN =
    rawFQN
        |> Maybe.map (\n -> [ string "namespace" n ])
        |> Maybe.withDefault []
        |> serverUrl [ "list" ]


definitionUrl : List String -> String
definitionUrl hashes =
    hashes
        |> List.map (string "names")
        |> serverUrl [ "getDefinition" ]



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
