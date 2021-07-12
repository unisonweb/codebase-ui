module Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , navigateToByReference
    , navigateToLatest
    , toUrlString
    )

import Browser.Navigation as Nav
import Definition.Reference as Reference exposing (Reference(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified exposing (HashQualified(..))
import List.Nonempty as NEL
import Url exposing (Url)
import Url.Builder exposing (relative)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



{-

   Routing
   =======

   URL Scheme
   ----------

   /[latest|:namespace-hash]/[namespaces|types|terms]/[:definition-name|:definition-hash]


   Relative examples
   -----------------

   Top level of a Codebase:          /
   Top level of a Codebase:          /latest
   Namespace as Workspace:           /latest/namespaces/base/List
   Definitions:                      /latest/[types|terms]/base/List
   Ambiguous definitions:            /latest/[types|terms]/base/List@je2wR6


   Absolute examples
   -----------------

   Namespace as Workspace:           /latest/namespaces/@785shikvuihsdfd
   Namespace as Workspace:           /@785shikvuihsdfd
   Definitions:                      /@785shikvuihsdfd/[types|terms]/base/List
   Ambiguous definitions:            /@785shikvuihsdfd/[types|terms]/Nonempty/map@dkqA42
   Definitions outside of Workspace: /@785shikvuihsdfd/[types|terms]/@jf615sgdkvuihskrt

-}


type NamespaceReference
    = Latest
    | NameReference FQN
    | HashReference Hash


type Route
    = Namespace NamespaceReference
    | ByReference NamespaceReference Reference



-- CREATE


urlParser : Parser (Route -> a) a
urlParser =
    oneOf
        [ Parser.map (Namespace Latest) Parser.top
        , Parser.map (Namespace Latest) (s "latest")
        , Parser.map Namespace (s "latest" </> s "namespaces" </> Hash.urlParser |> Parser.map HashReference)
        , Parser.map Namespace (s "latest" </> s "namespaces" </> FQN.urlParser |> Parser.map NameReference)
        , Parser.map (ByReference Latest) (s "latest" </> s "types" </> Reference.urlParser TypeReference)
        , Parser.map (ByReference Latest) (s "latest" </> s "terms" </> Reference.urlParser TermReference)
        , Parser.map (ByReference Latest) (s "latest" </> s "ability-constructors" </> Reference.urlParser AbilityConstructorReference)
        , Parser.map (ByReference Latest) (s "latest" </> s "data-constructors" </> Reference.urlParser DataConstructorReference)
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
        |> Maybe.withDefault (Namespace Latest)



-- TRANSFORM


toReference : Route -> Reference -> Route
toReference oldRoute ref =
    case oldRoute of
        Namespace nsRef ->
            case nsRef of
                Latest ->
                    ByReference Latest ref

                NameReference _ ->
                    ByReference Latest ref

                HashReference hash ->
                    ByReference (HashReference hash) ref

        ByReference within _ ->
            ByReference within ref


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

        namespaceReferenceToPath nsRef =
            case nsRef of
                Latest ->
                    [ "latest" ]

                NameReference fqn ->
                    "latest" :: NEL.toList (FQN.segments fqn)

                HashReference hash ->
                    [ "@" ++ Hash.toString hash ]

        path =
            case route of
                Namespace nsRef ->
                    namespaceReferenceToPath nsRef

                ByReference nsRef ref ->
                    case ref of
                        TypeReference hq ->
                            namespaceReferenceToPath nsRef ++ ("types" :: hqToPath hq)

                        TermReference hq ->
                            namespaceReferenceToPath nsRef ++ ("terms" :: hqToPath hq)

                        AbilityConstructorReference hq ->
                            namespaceReferenceToPath nsRef ++ ("ability-constructors" :: hqToPath hq)

                        DataConstructorReference hq ->
                            namespaceReferenceToPath nsRef ++ ("data-constructors" :: hqToPath hq)
    in
    relative path []



-- EFFECTS


navigate : Nav.Key -> Route -> Cmd msg
navigate navKey route =
    route
        |> toUrlString
        |> Nav.pushUrl navKey


navigateToLatest : Nav.Key -> Cmd msg
navigateToLatest navKey =
    navigate navKey (Namespace Latest)


navigateToByReference : Nav.Key -> Route -> Reference -> Cmd msg
navigateToByReference navKey currentRoute reference =
    navigate navKey (toReference currentRoute reference)
