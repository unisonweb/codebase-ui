module Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , navigateToByReference
    , navigateToLatest
    , navigateToTerm
    , navigateToType
    , relativeTo
    , toTerm
    , toType
    , toUrlString
    )

import Browser.Navigation as Nav
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified exposing (HashQualified(..))
import List.Nonempty as NEL
import RelativeTo exposing (RelativeTo)
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)
import Workspace.Reference as Reference exposing (Reference(..))



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
    | ByReference RelativeTo Reference



-- CREATE


urlParser : Parser (Route -> a) a
urlParser =
    oneOf
        [ Parser.map (Namespace Latest) Parser.top
        , Parser.map (Namespace Latest) (s "latest")
        , Parser.map Namespace (s "latest" </> s "namespaces" </> Hash.urlParser |> Parser.map HashReference)
        , Parser.map Namespace (s "latest" </> s "namespaces" </> FQN.urlParser |> Parser.map NameReference)
        , Parser.map (ByReference RelativeTo.Codebase) (s "latest" </> s "types" </> Reference.urlParser TypeReference)
        , Parser.map (ByReference RelativeTo.Codebase) (s "latest" </> s "terms" </> Reference.urlParser TermReference)
        ]


fromUrl : Url -> Route
fromUrl url =
    url
        |> Parser.parse urlParser
        |> Maybe.withDefault (Namespace Latest)



-- HELPERS


relativeTo : Route -> Maybe RelativeTo
relativeTo route =
    case route of
        Namespace ref ->
            case ref of
                Latest ->
                    Just RelativeTo.Codebase

                NameReference _ ->
                    Nothing

                HashReference h ->
                    Just (RelativeTo.Namespace h)

        ByReference relTo _ ->
            Just relTo



-- TRANSFORM


toReference : Route -> Reference -> Route
toReference oldRoute ref =
    case oldRoute of
        Namespace nsRef ->
            case nsRef of
                Latest ->
                    ByReference RelativeTo.Codebase ref

                NameReference _ ->
                    ByReference RelativeTo.Codebase ref

                HashReference hash ->
                    ByReference (RelativeTo.Namespace hash) ref

        ByReference within _ ->
            ByReference within ref


toType : Route -> HashQualified -> Route
toType oldRoute hq =
    toReference oldRoute (TypeReference hq)


toTerm : Route -> HashQualified -> Route
toTerm oldRoute hq =
    toReference oldRoute (TermReference hq)


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

        path =
            case route of
                Namespace ref ->
                    case ref of
                        Latest ->
                            [ "latest" ]

                        NameReference fqn ->
                            "latest" :: NEL.toList (FQN.segments fqn)

                        HashReference hash ->
                            [ "@" ++ Hash.toString hash ]

                ByReference relTo ref ->
                    case ref of
                        TypeReference hq ->
                            [ RelativeTo.toUrlPath relTo, "types" ] ++ hqToPath hq

                        TermReference hq ->
                            [ RelativeTo.toUrlPath relTo, "terms" ] ++ hqToPath hq
    in
    absolute path []



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
    case reference of
        TypeReference hq ->
            navigateToType navKey currentRoute hq

        TermReference hq ->
            navigateToTerm navKey currentRoute hq


navigateToType : Nav.Key -> Route -> HashQualified -> Cmd msg
navigateToType navKey currentRoute hq =
    navigate navKey (toType currentRoute hq)


navigateToTerm : Nav.Key -> Route -> HashQualified -> Cmd msg
navigateToTerm navKey currentRoute hq =
    navigate navKey (toTerm currentRoute hq)
