module Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , navigateToByReference
    , navigateToLatestCodebase
    , navigateToPerspective
    , perspectiveParams
    , toUrlString
    )

import Browser.Navigation as Nav
import Definition.Reference as Reference exposing (Reference(..))
import FullyQualifiedName as FQN
import Hash
import HashQualified exposing (HashQualified(..))
import List.Nonempty as NEL
import Perspective exposing (CodebasePerspectiveParam(..), PerspectiveParams(..))
import Url exposing (Url)
import Url.Builder exposing (relative)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



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
    | ByReference PerspectiveParams Reference



-- CREATE


urlParser : Parser (Route -> a) a
urlParser =
    let
        relativeCodebase =
            Perspective (ByCodebase Relative)

        relativePerspective =
            ByNamespace Relative >> Perspective

        relativeByReference =
            ByReference (ByCodebase Relative)

        absoluteCodebase =
            Absolute >> ByCodebase >> Perspective

        absolutePerspective codebaseHash namespaceFqn =
            Perspective (ByNamespace (Absolute codebaseHash) namespaceFqn)

        absoluteByReference codebaseHash definitionRef =
            ByReference (ByCodebase (Absolute codebaseHash)) definitionRef

        absoluteByReferenceAndNamespace codebaseHash namespaceFqn definitionRef =
            ByReference (ByNamespace (Absolute codebaseHash) namespaceFqn) definitionRef
    in
    oneOf
        [ -- Relative Routes
          Parser.map relativeCodebase Parser.top
        , Parser.map relativeCodebase (s "latest")
        , Parser.map relativePerspective (s "latest" </> s "namespaces" </> FQN.urlParser)
        , Parser.map relativeByReference (s "latest" </> s "types" </> Reference.urlParser TypeReference)
        , Parser.map relativeByReference (s "latest" </> s "terms" </> Reference.urlParser TermReference)
        , Parser.map relativeByReference (s "latest" </> s "ability-constructors" </> Reference.urlParser AbilityConstructorReference)
        , Parser.map relativeByReference (s "latest" </> s "data-constructors" </> Reference.urlParser DataConstructorReference)

        -- Absolute Routes
        , Parser.map absoluteCodebase Hash.urlParser
        , Parser.map absolutePerspective (Hash.urlParser </> s "namespaces" </> FQN.urlParser)
        , Parser.map absoluteByReferenceAndNamespace (Hash.urlParser </> s "namespaces" </> FQN.urlParser </> s "-" </> s "types" </> Reference.urlParser TypeReference)
        , Parser.map absoluteByReferenceAndNamespace (Hash.urlParser </> s "namespaces" </> FQN.urlParser </> s "-" </> s "terms" </> Reference.urlParser TermReference)
        , Parser.map absoluteByReferenceAndNamespace (Hash.urlParser </> s "namespaces" </> FQN.urlParser </> s "-" </> s "ability-constructors" </> Reference.urlParser AbilityConstructorReference)
        , Parser.map absoluteByReferenceAndNamespace (Hash.urlParser </> s "namespaces" </> FQN.urlParser </> s "-" </> s "data-constructors" </> Reference.urlParser DataConstructorReference)
        , Parser.map absoluteByReference (Hash.urlParser </> s "types" </> Reference.urlParser TypeReference)
        , Parser.map absoluteByReference (Hash.urlParser </> s "terms" </> Reference.urlParser TermReference)
        , Parser.map absoluteByReference (Hash.urlParser </> s "ability-constructors" </> Reference.urlParser AbilityConstructorReference)
        , Parser.map absoluteByReference (Hash.urlParser </> s "data-constructors" </> Reference.urlParser DataConstructorReference)
        ]


fromUrl : String -> Url -> Route
fromUrl basePath url =
    let
        stripBasePath u =
            if basePath == "/" then
                u

            else
                { u | path = String.replace basePath "" u.path }
    in
    url
        |> stripBasePath
        |> Parser.parse urlParser
        |> Maybe.withDefault (Perspective (ByCodebase Relative))



-- HELPERS


perspectiveParams : Route -> PerspectiveParams
perspectiveParams route =
    case route of
        Perspective nsRef ->
            nsRef

        ByReference nsRef _ ->
            nsRef



-- TRANSFORM


toByReference : Route -> Reference -> Route
toByReference oldRoute ref =
    ByReference (perspectiveParams oldRoute) ref


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

                ByReference pp ref ->
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


navigateToLatestCodebase : Nav.Key -> Cmd msg
navigateToLatestCodebase navKey =
    navigateToPerspective navKey (ByCodebase Relative)


navigateToByReference : Nav.Key -> Route -> Reference -> Cmd msg
navigateToByReference navKey currentRoute reference =
    navigate navKey (toByReference currentRoute reference)
