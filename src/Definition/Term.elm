module Definition.Term exposing
    ( Term(..)
    , TermCategory(..)
    , TermDetail
    , TermListing
    , TermSignature(..)
    , TermSource(..)
    , TermSummary
    , decodeSignature
    , decodeTermCategory
    , decodeTermSource
    , isBuiltin
    , isBuiltinSource
    , termSignature
    )

import Definition.Info exposing (Info)
import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (at, string)
import Json.Decode.Extra exposing (when)
import Syntax exposing (Syntax)


type TermCategory
    = PlainTerm
    | TestTerm
    | DocTerm


type TermSource
    = Source TermSignature Syntax
    | Builtin TermSignature


type Term a
    = Term Hash TermCategory a


type TermSignature
    = TermSignature Syntax


type alias TermDetailFields d =
    { d | info : Info, source : TermSource }


type alias TermDetail d =
    Term (TermDetailFields d)


type alias TermSummary =
    Term
        { fqn : FQN
        , name : FQN
        , namespace : Maybe String
        , signature : TermSignature
        }


type alias TermListing =
    Term FQN



-- HELPERS


isBuiltin : TermDetail d -> Bool
isBuiltin (Term _ _ d) =
    isBuiltinSource d.source


isBuiltinSource : TermSource -> Bool
isBuiltinSource source =
    case source of
        Source _ _ ->
            False

        Builtin _ ->
            True


termSignature : TermSource -> TermSignature
termSignature source =
    case source of
        Source sig _ ->
            sig

        Builtin sig ->
            sig



-- JSON DECODERS


decodeTermCategory : List String -> Decode.Decoder TermCategory
decodeTermCategory tagPath =
    let
        tag =
            at tagPath string
    in
    Decode.oneOf
        [ when tag ((==) "Test") (Decode.succeed TestTerm)
        , when tag ((==) "Doc") (Decode.succeed DocTerm)
        , Decode.succeed PlainTerm
        ]


decodeSignature : List String -> Decode.Decoder TermSignature
decodeSignature signaturePath =
    Decode.map TermSignature (at signaturePath Syntax.decode)


decodeTermSource : List String -> List String -> List String -> Decode.Decoder TermSource
decodeTermSource tagPath signaturePath sourcePath =
    let
        tag =
            at tagPath string

        decodeUserObject =
            Decode.map2 Source
                (decodeSignature signaturePath)
                (at sourcePath Syntax.decode)

        decodeBuiltin =
            Decode.map Builtin (decodeSignature signaturePath)
    in
    Decode.oneOf
        [ when tag ((==) "UserObject") decodeUserObject
        , when tag ((==) "BuiltinObject") decodeBuiltin
        ]
