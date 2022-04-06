module UnisonLocal.Api exposing
    ( codebaseApiEndpointToEndpointUrl
    , codebaseHash
    , namespace
    )

import Code.CodebaseApi as CodebaseApi
import Code.EntityId as EntityId
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.Syntax as Syntax
import Lib.Api as Api exposing (EndpointUrl(..))
import Regex
import Url.Builder exposing (QueryParameter, int, string)


codebaseHash : EndpointUrl
codebaseHash =
    EndpointUrl [ "list" ] [ string "namespace" "." ]


namespace : Perspective -> FQN -> EndpointUrl
namespace perspective fqn =
    let
        queryParams =
            [ rootBranch (Perspective.codebaseHash perspective) ]
    in
    EndpointUrl [ "namespaces", FQN.toString fqn ] queryParams


codebaseApiEndpointToEndpointUrl : CodebaseApi.CodebaseEndpoint -> EndpointUrl
codebaseApiEndpointToEndpointUrl cbEndpoint =
    case cbEndpoint of
        CodebaseApi.Find { perspective, withinFqn, limit, sourceWidth, query } ->
            let
                params =
                    case withinFqn of
                        Just fqn ->
                            [ rootBranch (Perspective.codebaseHash perspective), relativeTo fqn ]

                        Nothing ->
                            perspectiveToQueryParams perspective

                width =
                    case sourceWidth of
                        Syntax.Width w ->
                            w
            in
            EndpointUrl
                [ "find" ]
                ([ int "limit" limit
                 , int "renderWidth" width
                 , string "query" query
                 ]
                    ++ params
                )

        CodebaseApi.Browse { perspective, namespaceId } ->
            let
                namespace_ =
                    namespaceId |> Maybe.map EntityId.toString |> Maybe.withDefault "."
            in
            EndpointUrl [ "list" ] (string "namespace" namespace_ :: perspectiveToQueryParams perspective)

        CodebaseApi.Definition { perspective, definitionId } ->
            let
                re =
                    Maybe.withDefault Regex.never (Regex.fromString "#[d|a|](\\d+)$")

                stripConstructorPositionFromHash =
                    Regex.replace re (always "")
            in
            [ EntityId.toString definitionId ]
                |> List.map stripConstructorPositionFromHash
                |> List.map (string "names")
                |> (\names -> EndpointUrl [ "getDefinition" ] (names ++ perspectiveToQueryParams perspective))



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
