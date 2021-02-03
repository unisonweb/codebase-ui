module Api exposing (..)

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
