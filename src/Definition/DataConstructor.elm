module Definition.DataConstructor exposing
    ( DataConstructor(..)
    , DataConstructorDetail
    , DataConstructorListing
    , DataConstructorSource(..)
    , DataConstructorSummary
    , decodeSignature
    , decodeSource
    , isDataConstructorHash
    )

import Definition.Info exposing (Info)
import Definition.Term as Term exposing (TermSignature)
import Definition.Type as Type exposing (TypeSource)
import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode
import Regex
import Syntax exposing (Syntax)


type DataConstructorSource
    = Source Syntax
    | Builtin


type DataConstructor a
    = DataConstructor Hash a


type alias DataConstructorDetail =
    DataConstructor { info : Info, source : TypeSource }


type alias DataConstructorSummary =
    DataConstructor
        { fqn : FQN
        , name : String
        , namespace : Maybe String
        , signature : TermSignature
        }


type alias DataConstructorListing =
    DataConstructor FQN



-- HELPERS


isDataConstructorHash : Hash -> Bool
isDataConstructorHash hash =
    let
        dataConstructorSuffix =
            Maybe.withDefault Regex.never (Regex.fromString "#d(\\d+)$")
    in
    hash |> Hash.toString |> Regex.contains dataConstructorSuffix



-- JSON DECODERS


decodeSource : Decode.Decoder TypeSource
decodeSource =
    Type.decodeSource


decodeSignature : Decode.Decoder TermSignature
decodeSignature =
    Term.decodeSignature
