module UnisonShare.Route exposing
    ( ProjectRoute(..)
    , Route(..)
    , forProject
    , forUser
    , fromUrl
    , navigate
    , navigateToCurrentPerspective
    , navigateToDefinition
    , navigateToLatestCodebase
    , navigateToPerspective
    , navigateToProject
    , navigateToUser
    , navigateToUsername
    , perspectiveParams
    , replacePerspective
    , toDefinition
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
import Project exposing (Project)
import Route.Parsers as RP exposing (b, reference, s, slash)
import UnisonShare.User as User exposing (User)
import Url exposing (Url)
import Url.Builder exposing (relative)



{-

   URL Scheme for Projects
   =======================


   Directly on the codebase
   /[latest|:codebase-hash]/[namespaces|types|terms]/[:namespace-name|:definition-name|:definition-hash]


   Within a namespace
   /[latest|:codebase-hash]/[namespaces]/[:namespace-name]/-/[types|terms]/[:definition-name|:definition-hash]

   TODO:
     /projects/[:project-name]/-/[namespaces|types|terms]/[:definition-name|:definition-hash]
     /projects/[:project-name]/-/[namespaces]/[:namespace-name]/-/[types|terms]/[:definition-name|:definition-hash]

   Relative examples
   -----------------

   Top level of a Codebase:                     /
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


type ProjectRoute
    = ProjectRoot
    | ProjectDefinition Reference


type Route
    = Catalog
    | User User.Username
    | Project PerspectiveParams ProjectRoute


updatePerspectiveParams : Route -> PerspectiveParams -> Route
updatePerspectiveParams route params =
    case route of
        Catalog ->
            Catalog

        User username_ ->
            User username_

        Project _ ProjectRoot ->
            Project params ProjectRoot

        Project _ (ProjectDefinition ref) ->
            Project params (ProjectDefinition ref)



-- PARSER ---------------------------------------------------------------------


catalog : Parser Route
catalog =
    succeed Catalog |. slash |. s "catalog"


user : Parser Route
user =
    succeed User |. slash |. s "users" |. slash |= username |. end


username : Parser User.Username
username =
    let
        handleMaybe mUsername =
            case mUsername of
                Just u ->
                    Parser.succeed u

                Nothing ->
                    Parser.problem "Invalid username"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map User.usernameFromString
        |> Parser.andThen handleMaybe


perspective : Parser Route
perspective =
    succeed (\pp -> Project pp ProjectRoot) |. slash |= RP.perspectiveParams |. end


definition : Parser Route
definition =
    succeed (\pp r -> Project pp (ProjectDefinition r)) |. slash |= RP.perspectiveParams |. slash |= reference |. end


toRoute : Parser Route
toRoute =
    oneOf [ b catalog, b user, b perspective, b definition ]


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
            Result.withDefault Catalog (Parser.run toRoute url_)
    in
    url
        |> .path
        |> stripBasePath
        |> ensureSlashPrefix
        |> parse



-- HELPERS --------------------------------------------------------------------


perspectiveParams : Route -> Maybe PerspectiveParams
perspectiveParams route =
    case route of
        Project pp _ ->
            Just pp

        _ ->
            Nothing



-- Create


forProject : Project a -> Route
forProject project_ =
    let
        fqn =
            FQN.cons (Project.ownerToString project_.owner) project_.name
    in
    Project (Perspective.ByNamespace Relative fqn) ProjectRoot


forUser : User a -> Route
forUser user_ =
    User user_.username



-- TRANSFORM


toDefinition : Route -> Reference -> Route
toDefinition oldRoute ref =
    let
        params =
            oldRoute
                |> perspectiveParams
                |> Maybe.withDefault (ByCodebase Relative)
    in
    Project params (ProjectDefinition ref)


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

                -- Currently the model supports Absolute URLs (aka Permalinks),
                -- but we don't use it since Unison Share does not support any
                -- history, meaning that everytime we deploy Unison Share, the
                -- previous versions of the codebase are lost.
                -- It's fully intended for this feature to be brought back
                ByNamespace (Absolute hash) fqn ->
                    if includeNamespacesSuffix then
                        Hash.toUrlString hash :: "namespaces" :: NEL.toList (FQN.segments fqn) ++ [ namespaceSuffix ]

                    else
                        Hash.toUrlString hash :: "namespaces" :: NEL.toList (FQN.segments fqn)

        path =
            case route of
                Catalog ->
                    [ "catalog" ]

                User username_ ->
                    [ "users", User.usernameToString username_ ]

                Project pp ProjectRoot ->
                    perspectiveParamsToPath pp False

                Project pp (ProjectDefinition ref) ->
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


navigateToProject : Nav.Key -> Project a -> Cmd msg
navigateToProject navKey project =
    navigate navKey (forProject project)


navigateToUser : Nav.Key -> User.User a -> Cmd msg
navigateToUser navKey user_ =
    navigate navKey (forUser user_)


navigateToUsername : Nav.Key -> User.Username -> Cmd msg
navigateToUsername navKey username_ =
    navigate navKey (User username_)



-- TODO: this should go away in UnisonShare


navigateToPerspective : Nav.Key -> PerspectiveParams -> Cmd msg
navigateToPerspective navKey perspectiveParams_ =
    navigate navKey (Project perspectiveParams_ ProjectRoot)


navigateToCurrentPerspective : Nav.Key -> Route -> Cmd msg
navigateToCurrentPerspective navKey oldRoute =
    let
        params =
            oldRoute
                |> perspectiveParams
                |> Maybe.withDefault (ByCodebase Relative)
    in
    navigateToPerspective navKey params


navigateToLatestCodebase : Nav.Key -> Cmd msg
navigateToLatestCodebase navKey =
    navigateToPerspective navKey (ByCodebase Relative)


navigateToDefinition : Nav.Key -> Route -> Reference -> Cmd msg
navigateToDefinition navKey currentRoute reference =
    navigate navKey (toDefinition currentRoute reference)



-- TODO: This should go away in UnisonShare


replacePerspective : Nav.Key -> PerspectiveParams -> Route -> Cmd msg
replacePerspective navKey perspectiveParams_ oldRoute =
    let
        newRoute =
            case oldRoute of
                Project _ ProjectRoot ->
                    Project perspectiveParams_ ProjectRoot

                Project _ (ProjectDefinition ref) ->
                    Project perspectiveParams_ (ProjectDefinition ref)

                _ ->
                    oldRoute
    in
    navigate navKey newRoute
