module Definition.Type exposing
    ( Type(..)
    , TypeCategory(..)
    , TypeDetail
    , TypeListing
    , TypeSource(..)
    , TypeSummary
    , decodeFQN
    , decodeHash
    , decodeTypeCategory
    , decodeTypeSource
    )

import Definition.Info exposing (Info)
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Json.Decode as Decode exposing (field, string)
import Json.Decode.Extra exposing (when)
import Syntax exposing (Syntax)


type TypeCategory
    = DataType
    | AbilityType


type TypeSource
    = Source Syntax
    | Builtin


type Type a
    = Type Hash TypeCategory a


type alias TypeDetail =
    Type { info : Info, source : TypeSource }


type alias TypeSummary =
    Type
        { fqn : FQN
        , name : String
        , namespace : Maybe String
        , source : TypeSource
        }


type alias TypeListing =
    Type FQN



-- JSON DECODERS


decodeTypeCategory : String -> Decode.Decoder TypeCategory
decodeTypeCategory tagFieldName =
    Decode.oneOf
        [ when (field tagFieldName string) ((==) "Data") (Decode.succeed DataType)
        , when (field tagFieldName string) ((==) "Ability") (Decode.succeed AbilityType)
        ]


decodeTypeSource : Decode.Decoder TypeSource
decodeTypeSource =
    let
        decodeUserObject =
            Decode.map Source (field "contents" Syntax.decode)
    in
    Decode.oneOf
        [ when (field "tag" string) ((==) "UserObject") decodeUserObject
        , when (field "tag" string) ((==) "BuiltinObject") (Decode.succeed Builtin)
        ]


decodeHash : Decode.Decoder Hash
decodeHash =
    field "typeHash" Hash.decode


decodeFQN : Decode.Decoder FQN
decodeFQN =
    field "typeName" FQN.decode
