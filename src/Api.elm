module Api exposing (errorToString, find, getDefinition, list)

import Env
import Http
import Syntax
import Url.Builder exposing (QueryParameter, absolute, int, string)


{-| TODO: Be more explicit about Root |
-}
list : Maybe String -> String
list rawFQN =
    rawFQN
        |> Maybe.map (\n -> [ string "namespace" n ])
        |> Maybe.withDefault []
        |> serverUrl [ "list" ]


getDefinition : List String -> String
getDefinition fqnsOrHashes =
    fqnsOrHashes
        |> List.map (string "names")
        |> serverUrl [ "getDefinition" ]


find : Int -> Syntax.Width -> String -> String
find limit (Syntax.Width sourceWidth) query =
    serverUrl
        [ "find" ]
        [ int "limit" limit
        , int "renderWidth" sourceWidth
        , string "query" query
        ]



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



-- PRIVATE URL HELPERS


serverUrl : List String -> List QueryParameter -> String
serverUrl path queryParams =
    let
        url =
            absolute ("api" :: path) queryParams
    in
    if String.contains "?" url then
        url ++ "&" ++ Env.apiToken

    else
        url ++ "?" ++ Env.apiToken
