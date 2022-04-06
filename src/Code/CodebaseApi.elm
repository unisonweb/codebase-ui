module Code.CodebaseApi exposing (..)

import Code.EntityId exposing (EntityId)
import Code.FullyQualifiedName exposing (FQN)
import Code.Perspective exposing (Perspective)
import Code.Syntax as Syntax
import Lib.Api as Api



{-

   CodebaseApi
   ===========

   The CodebaseApi module, describes the endpoints used for the Code library to
   connect to a codebase, but is merely that; a description. Consumers of the Code
   library will need to provide an implementation of `ToApiEndpointUrl` in order
   to perform the actual HTTP requests.

-}


type CodebaseEndpoint
    = Find
        { perspective : Perspective
        , withinFqn : Maybe FQN
        , limit : Int
        , sourceWidth : Syntax.Width
        , query : String
        }
    | Browse { perspective : Perspective, namespaceId : Maybe EntityId }
    | Definition { perspective : Perspective, definitionId : EntityId }


type alias ToApiEndpointUrl =
    CodebaseEndpoint -> Api.EndpointUrl
