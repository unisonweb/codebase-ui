module Code.Config exposing (..)

import Code.CodebaseApi exposing (ToApiEndpointUrl)
import Code.Perspective exposing (Perspective)
import Lib.HttpApi exposing (ApiBasePath)
import Lib.OperatingSystem exposing (OperatingSystem)


type alias Config =
    { operatingSystem : OperatingSystem
    , perspective : Perspective
    , toApiEndpointUrl : ToApiEndpointUrl
    , apiBasePath : ApiBasePath
    }
