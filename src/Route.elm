module Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , navigateToByReference
    , navigateToCurrentPerspective
    , navigateToLatestCodebase
    , navigateToPerspective
    , perspectiveParams
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


fromUrl : String -> Url -> Route
fromUrl basePath url =
    let
        stripBasePath u =
            if basePath == "/" then
                u

            else
                { u | path = String.replace basePath "" u.path }

        parse url_ =
            Result.withDefault (Perspective (ByCodebase Relative)) (Parser.run toRoute url_)
    in
    url
        |> stripBasePath
        |> .path
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
                    NEL.toList (FQN.segments fqn)

                HashOnly h ->
                    [ Hash.toUrlString h ]

                HashQualified fqn h ->
                    String.split "/" (FQN.toUrlString fqn ++ Hash.toUrlString h)

        perspectiveParamsToPath pp =
            case pp of
                ByCodebase Relative ->
                    [ "latest" ]

                ByCodebase (Absolute hash) ->
                    [ Hash.toUrlString hash ]

                ByNamespace Relative fqn ->
                    "latest" :: NEL.toList (FQN.segments fqn)

                ByNamespace (Absolute hash) fqn ->
                    [ Hash.toUrlString hash, "namespaces" ] ++ NEL.toList (FQN.segments fqn)

        path =
            case route of
                Perspective pp ->
                    perspectiveParamsToPath pp

                Definition pp ref ->
                    case ref of
                        TypeReference hq ->
                            perspectiveParamsToPath pp ++ ("types" :: hqToPath hq)

                        TermReference hq ->
                            perspectiveParamsToPath pp ++ ("terms" :: hqToPath hq)

                        AbilityConstructorReference hq ->
                            perspectiveParamsToPath pp ++ ("ability-constructors" :: hqToPath hq)

                        DataConstructorReference hq ->
                            perspectiveParamsToPath pp ++ ("data-constructors" :: hqToPath hq)
    in
    relative path []



-- EFFECTS


navigate : Nav.Key -> Route -> Cmd msg
navigate navKey route =
    route
        |> toUrlString
        |> Nav.pushUrl navKey


navigateToPerspective : Nav.Key -> PerspectiveParams -> Cmd msg
navigateToPerspective navKey pp =
    navigate navKey (Perspective pp)


navigateToCurrentPerspective : Nav.Key -> Route -> Cmd msg
navigateToCurrentPerspective navKey oldRoute =
    navigateToPerspective navKey (perspectiveParams oldRoute)


navigateToLatestCodebase : Nav.Key -> Cmd msg
navigateToLatestCodebase navKey =
    navigateToPerspective navKey (ByCodebase Relative)


navigateToByReference : Nav.Key -> Route -> Reference -> Cmd msg
navigateToByReference navKey currentRoute reference =
    navigate navKey (toDefinition currentRoute reference)
