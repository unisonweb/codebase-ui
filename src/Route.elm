module Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , navigateToByReference
    , navigateToCurrentPerspective
    , navigateToLatestCodebase
    , navigateToPerspective
    , perspectiveParams
    , replacePerspective
    , toRoute
    , toUrlString
    , updatePerspectiveParams
    )

import Browser.Navigation as Nav
import Definition.Reference exposing (Reference(..))
import FullyQualifiedName as FQN
import Hash
import HashQualified exposing (HashQualified(..))
import List.Nonempty as NEL
import Parser exposing ((|.), (|=), Parser, end, oneOf, succeed)
import Perspective exposing (CodebasePerspectiveParam(..), PerspectiveParams(..))
import Route.Parsers as RP exposing (b, reference, slash)
import Url exposing (Url)
import Url.Builder exposing (relative)



{-

   Routing
   =======

   URL Scheme
   ----------

   Directly on the codebase
   /[latest|:codebase-hash]/[namespaces|types|terms]/[:namespace-name|:definition-name|:definition-hash]


   Within a namespace
   /[latest|:codebase-hash]/[namespaces]/[:namespace-name]/-/[types|terms]/[:definition-name|:definition-hash]


   Relative examples
   -----------------

   Top level of a Codebase:                       /
   Top level of a Codebase:                     /latest
   With namespace context:                      /latest/namespaces/base/List
   Definitions:                                 /latest/[types|terms]/base/List/map
   Disambiguated definitions:                   /latest/[types|terms]/base/List@je2wR6
   Definitions within namespace:                /latest/namespaces/base/List/-/[types|terms]/map
   Disambiguated definitions within namespace:  /latest/namespaces/base/List/-/[types|terms]/map@je2wR6


   Absolute examples
   -----------------

   Definitions:                                 /@785shikvuihsdfd/[types|terms]/@jf615sgdkvuihskrt
   Disambiguated definitions:                   /@785shikvuihsdfd/[types|terms]/Nonempty/map@dkqA42
   With namespace context:                      /@785shikvuihsdfd/namespaces/base/List
   Definitions within namespace:                /@785shikvuihsdfd/namespaces/base/List/-/[types|terms]/base/List/map
   Disambiguated definitions within namespace:  /@785shikvuihsdfd/namespaces/base/List/-/[types|terms]/Nonempty/map@dkqA42


   Note: @785shikvuihsdfd here refers to the hash of the codebase

-}


type Route
    = Perspective PerspectiveParams
    | Definition PerspectiveParams Reference


updatePerspectiveParams : Route -> PerspectiveParams -> Route
updatePerspectiveParams route params =
    case route of
        Perspective _ ->
            Perspective params

        Definition _ ref ->
            Definition params ref



-- PARSER ---------------------------------------------------------------------


perspective : Parser Route
perspective =
    succeed Perspective |. slash |= RP.perspectiveParams |. end


definition : Parser Route
definition =
    succeed Definition |. slash |= RP.perspectiveParams |. slash |= reference |. end


toRoute : Parser Route
toRoute =
    oneOf [ b perspective, b definition ]


{-| In environments like Unison Local, the UI is served with a base path

This means that a route to a definition might look like:

  - "/:some-token/ui/latest/terms/base/List/map"
    (where "/:some-token/ui/" is the base path.)

The base path is determined outside of the Elm app using the <base> tag in the
<head> section of the document. The Browser uses this tag to prefix all links.

The base path must end in a slash for links to work correctly, but our parser
expects a path to starts with a slash. When parsing the URL we thus pre-process
the path to strip the base path and ensure a slash prefix before we parse.

-}
fromUrl : String -> Url -> Route
fromUrl basePath url =
    let
        stripBasePath path =
            if basePath == "/" then
                path

            else
                String.replace basePath "" path

        ensureSlashPrefix path =
            if String.startsWith "/" path then
                path

            else
                "/" ++ path

        parse url_ =
            Result.withDefault (Perspective (ByCodebase Relative)) (Parser.run toRoute url_)
    in
    url
        |> .path
        |> stripBasePath
        |> ensureSlashPrefix
        |> parse



-- HELPERS --------------------------------------------------------------------


perspectiveParams : Route -> PerspectiveParams
perspectiveParams route =
    case route of
        Perspective nsRef ->
            nsRef

        Definition nsRef _ ->
            nsRef



-- TRANSFORM


toDefinition : Route -> Reference -> Route
toDefinition oldRoute ref =
    Definition (perspectiveParams oldRoute) ref


toUrlString : Route -> String
toUrlString route =
    let
        hqToPath hq =
            case hq of
                NameOnly fqn ->
                    fqn |> FQN.toUrlSegments |> NEL.toList

                HashOnly h ->
                    [ Hash.toUrlString h ]

                HashQualified fqn h ->
                    NEL.toList (FQN.toUrlSegments fqn) ++ [ Hash.toUrlString h ]

        namespaceSuffix =
            ";"

        -- used to mark the end of a namespace FQN
        perspectiveParamsToPath pp includeNamespacesSuffix =
            case pp of
                ByCodebase Relative ->
                    [ "latest" ]

                ByCodebase (Absolute hash) ->
                    [ Hash.toUrlString hash ]

                ByNamespace Relative fqn ->
                    if includeNamespacesSuffix then
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn) ++ [ namespaceSuffix ]

                    else
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn)

                ByNamespace (Absolute _) fqn ->
                    if includeNamespacesSuffix then
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn) ++ [ namespaceSuffix ]

                    else
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn)

        path =
            case route of
                Perspective pp ->
                    perspectiveParamsToPath pp False

                Definition pp ref ->
                    case ref of
                        TypeReference hq ->
                            perspectiveParamsToPath pp True ++ ("types" :: hqToPath hq)

                        TermReference hq ->
                            perspectiveParamsToPath pp True ++ ("terms" :: hqToPath hq)

                        AbilityConstructorReference hq ->
                            perspectiveParamsToPath pp True ++ ("ability-constructors" :: hqToPath hq)

                        DataConstructorReference hq ->
                            perspectiveParamsToPath pp True ++ ("data-constructors" :: hqToPath hq)
    in
    relative path []



-- EFFECTS


navigate : Nav.Key -> Route -> Cmd msg
navigate navKey route =
    route
        |> toUrlString
        |> Nav.pushUrl navKey


navigateToPerspective : Nav.Key -> PerspectiveParams -> Cmd msg
navigateToPerspective navKey perspectiveParams_ =
    navigate navKey (Perspective perspectiveParams_)


navigateToCurrentPerspective : Nav.Key -> Route -> Cmd msg
navigateToCurrentPerspective navKey oldRoute =
    navigateToPerspective navKey (perspectiveParams oldRoute)


navigateToLatestCodebase : Nav.Key -> Cmd msg
navigateToLatestCodebase navKey =
    navigateToPerspective navKey (ByCodebase Relative)


navigateToByReference : Nav.Key -> Route -> Reference -> Cmd msg
navigateToByReference navKey currentRoute reference =
    navigate navKey (toDefinition currentRoute reference)


replacePerspective : Nav.Key -> PerspectiveParams -> Route -> Cmd msg
replacePerspective navKey perspectiveParams_ oldRoute =
    let
        newRoute =
            case oldRoute of
                Perspective _ ->
                    Perspective perspectiveParams_

                Definition _ ref ->
                    Definition perspectiveParams_ ref
    in
    navigate navKey newRoute
