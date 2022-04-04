module Code.Definition.Type exposing
    ( Type(..)
    , TypeCategory(..)
    , TypeDetail
    , TypeListing
    , TypeSource(..)
    , TypeSummary
    , decodeTypeCategory
    , decodeTypeSource
    , isBuiltin
    , isBuiltinSource
    )

import Code.Definition.Info exposing (Info)
import Code.FullyQualifiedName exposing (FQN)
import Code.Hash exposing (Hash)
import Code.Syntax as Syntax exposing (Syntax)
import Json.Decode as Decode exposing (at, string)
import Json.Decode.Extra exposing (when)


type TypeCategory
    = DataType
    | AbilityType


type TypeSource
    = Source Syntax
    | Builtin


type Type a
    = Type Hash TypeCategory a


type alias TypeDetailFields d =
    { d | info : Info, source : TypeSource }


type alias TypeDetail d =
    Type (TypeDetailFields d)


type alias TypeSummary =
    Type
        { fqn : FQN
        , name : FQN
        , namespace : Maybe String
        , source : TypeSource
        }


type alias TypeListing =
    Type FQN



-- HELPERS


isBuiltin : TypeDetail d -> Bool
isBuiltin (Type _ _ d) =
    isBuiltinSource d.source


isBuiltinSource : TypeSource -> Bool
isBuiltinSource source =
    case source of
        Source _ ->
            False

        Builtin ->
            True



-- JSON DECODERS


decodeTypeCategory : List String -> Decode.Decoder TypeCategory
decodeTypeCategory tagPath =
    let
        tag =
            at tagPath string
    in
    Decode.oneOf
        [ when tag ((==) "Data") (Decode.succeed DataType)
        , when tag ((==) "Ability") (Decode.succeed AbilityType)
        ]


decodeTypeSource : List String -> List String -> Decode.Decoder TypeSource
decodeTypeSource tagPath sourcePath =
    let
        tag =
            at tagPath string

        decodeUserObject =
            Decode.map Source (at sourcePath Syntax.decode)
    in
    Decode.oneOf
        [ when tag ((==) "UserObject") decodeUserObject
        , when tag ((==) "BuiltinObject") (Decode.succeed Builtin)
        ]
