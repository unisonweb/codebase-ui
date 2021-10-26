module Definition.DataConstructor exposing
    ( DataConstructor(..)
    , DataConstructorDetail
    , DataConstructorListing
    , DataConstructorSource(..)
    , DataConstructorSummary
    , decodeSignature
    , decodeSource
    )

import Definition.Info exposing (Info)
import Definition.Term as Term exposing (TermSignature)
import Definition.Type as Type exposing (TypeSource)
import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode
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
        , name : FQN
        , namespace : Maybe String
        , signature : TermSignature
        }


type alias DataConstructorListing =
    DataConstructor FQN



-- JSON DECODERS


decodeSource : List String -> List String -> Decode.Decoder TypeSource
decodeSource =
    Type.decodeTypeSource


decodeSignature : List String -> Decode.Decoder TermSignature
decodeSignature =
    Term.decodeSignature
