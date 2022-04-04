module Code.Definition.DataConstructor exposing
    ( DataConstructor(..)
    , DataConstructorDetail
    , DataConstructorListing
    , DataConstructorSource(..)
    , DataConstructorSummary
    , decodeSignature
    , decodeSource
    )

import Code.Definition.Info exposing (Info)
import Code.Definition.Term as Term exposing (TermSignature)
import Code.Definition.Type as Type exposing (TypeSource)
import Code.FullyQualifiedName exposing (FQN)
import Code.Hash exposing (Hash)
import Code.Syntax exposing (Syntax)
import Json.Decode as Decode


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
