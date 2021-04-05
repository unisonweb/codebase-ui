module Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , navigateToLatest
    , navigateToTerm
    , navigateToType
    , toTerm
    , toType
    , toUrlString
    )

import Browser.Navigation as Nav
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified as HQ exposing (HashQualified(..))
import List.Nonempty as NEL
import Url exposing (Url)
import Url.Builder exposing (absolute)
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


type WithinNamespace
    = WithinLatest
    | WithinHash Hash


type Route
    = Namespace NamespaceReference
    | Type WithinNamespace HashQualified
    | Term WithinNamespace HashQualified



-- CREATE


urlParser : Parser (Route -> a) a
urlParser =
    oneOf
        [ Parser.map (Namespace Latest) Parser.top
        , Parser.map (Namespace Latest) (s "latest")
        , Parser.map Namespace (s "latest" </> s "namespaces" </> Hash.urlParser |> Parser.map HashReference)
        , Parser.map Namespace (s "latest" </> s "namespaces" </> FQN.urlParser |> Parser.map NameReference)
        , Parser.map (Type WithinLatest) (s "latest" </> s "types" </> HQ.urlParser)
        , Parser.map (Term WithinLatest) (s "latest" </> s "terms" </> HQ.urlParser)
        ]


fromUrl : Url -> Route
fromUrl url =
    url
        |> Parser.parse urlParser
        |> Maybe.withDefault (Namespace Latest)



-- TRANSFORM


toType : Route -> HashQualified -> Route
toType oldRoute hq =
    case oldRoute of
        Namespace ref ->
            case ref of
                Latest ->
                    Type WithinLatest hq

                NameReference _ ->
                    Type WithinLatest hq

                HashReference hash ->
                    Type (WithinHash hash) hq

        Type within _ ->
            Type within hq

        Term within _ ->
            Type within hq


toTerm : Route -> HashQualified -> Route
toTerm oldRoute hq =
    case oldRoute of
        Namespace ref ->
            case ref of
                Latest ->
                    Term WithinLatest hq

                NameReference _ ->
                    Term WithinLatest hq

                HashReference hash ->
                    Term (WithinHash hash) hq

        Type within _ ->
            Term within hq

        Term within _ ->
            Term within hq


toUrlString : Route -> String
toUrlString route =
    let
        withinToPath within =
            case within of
                WithinLatest ->
                    "latest"

                WithinHash h ->
                    Hash.toUrlString h

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

                Type within hq ->
                    [ withinToPath within, "types" ] ++ hqToPath hq

                Term within hq ->
                    [ withinToPath within, "terms" ] ++ hqToPath hq
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


navigateToType : Nav.Key -> Route -> HashQualified -> Cmd msg
navigateToType navKey currentRoute hq =
    navigate navKey (toType currentRoute hq)


navigateToTerm : Nav.Key -> Route -> HashQualified -> Cmd msg
navigateToTerm navKey currentRoute hq =
    navigate navKey (toTerm currentRoute hq)
