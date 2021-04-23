module Definition.Term exposing
    ( Term(..)
    , TermCategory(..)
    , TermDetail
    , TermListing
    , TermSignature(..)
    , TermSource(..)
    , TermSummary
    , decodeFQN
    , decodeHash
    , decodeTermCategory
    , decodeTermSignature
    )

import Definition.Info exposing (Info)
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (field, string)
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


type alias TermDetail =
    Term { info : Info, source : TermSource }


type alias TermSummary =
    Term
        { fqn : FQN
        , name : String
        , namespace : Maybe String
        , signature : TermSignature
        }


type alias TermListing =
    Term FQN



-- JSON DECODERS


decodeTermCategory : String -> Decode.Decoder TermCategory
decodeTermCategory tagFieldName =
    Decode.oneOf
        [ when (field tagFieldName string) ((==) "Test") (Decode.succeed TestTerm)
        , when (field tagFieldName string) ((==) "Doc") (Decode.succeed DocTerm)
        , Decode.succeed PlainTerm
        ]


decodeTermSignature : Decode.Decoder TermSignature
decodeTermSignature =
    Decode.map TermSignature (field "termType" Syntax.decode)


decodeHash : Decode.Decoder Hash
decodeHash =
    field "termHash" Hash.decode


decodeFQN : Decode.Decoder FQN
decodeFQN =
    field "termName" FQN.decode
