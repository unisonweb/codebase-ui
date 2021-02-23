module Syntax exposing (Syntax, SyntaxSegment(..), SyntaxType(..), decode, view)

import Hash exposing (Hash)
import Html exposing (Html, a, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (andThen, at, field)
import Json.Decode.Extra exposing (when)
import List.Nonempty as NEL
import Util


type Syntax
    = Syntax (NEL.Nonempty SyntaxSegment)


type SyntaxSegment
    = SyntaxSegment SyntaxType String


type SeqOp
    = Cons
    | Snoc
    | Concat


type HashQualified
    = NameOnly String
    | HashOnly Hash
    | HashQualified String Hash


type SyntaxType
    = NumericLiteral
    | TextLiteral
    | BytesLiteral
    | CharLiteral
    | BooleanLiteral
    | Blank
    | Var
    | Reference Hash
    | Referent Hash
      -- +:|:+|++
    | Op SeqOp
    | Constructor
      -- Ability constructor
    | Request
    | AbilityBraces
      -- let|handle|in|where|match|with|cases|->|if|then|else|and|or
    | ControlKeyword
      -- forall|->
    | TypeOperator
    | BindingEquals
    | TypeAscriptionColon
      -- type|ability
    | DataTypeKeyword
    | DataTypeParams
    | Unit
      -- unique
    | DataTypeModifier
      -- `use Foo bar` is keyword, prefix, suffix
    | UseKeyword
    | UsePrefix
    | UseSuffix
    | HashQualifier HashQualified
      -- ! '
    | DelayForceChar
      -- ? , ` [ ] @ |
      -- Currently not all commas in the pretty-print output are marked up as DelimiterChar - we miss
      -- out characters emitted by Pretty.hs helpers like Pretty.commas.
    | DelimiterChar
    | Parenthesis
    | LinkKeyword -- `typeLink` and `termLink`
      -- [: :] @[]
    | DocDelimiter
      -- the 'include' in @[include], etc
    | DocKeyword



-- VIEW


syntaxTypeToClassName : SyntaxType -> String
syntaxTypeToClassName sType =
    case sType of
        NumericLiteral ->
            "numberic-literal"

        TextLiteral ->
            "text-literal"

        BytesLiteral ->
            "bytes-literal"

        CharLiteral ->
            "char-literal"

        BooleanLiteral ->
            "boolean-literal"

        Blank ->
            "blank"

        Var ->
            "var"

        Reference _ ->
            "reference"

        Referent _ ->
            "referent"

        Op seqOp ->
            case seqOp of
                Cons ->
                    "op cons"

                Snoc ->
                    "op snoc"

                Concat ->
                    "op concat"

        Constructor ->
            "constructor"

        Request ->
            "request"

        AbilityBraces ->
            "ability-braces"

        ControlKeyword ->
            "control-keyword"

        TypeOperator ->
            "type-operator"

        BindingEquals ->
            "binding-equals"

        TypeAscriptionColon ->
            "type-ascription-colon"

        DataTypeKeyword ->
            "data-type-keyword"

        DataTypeParams ->
            "data-type-params"

        Unit ->
            "unit"

        DataTypeModifier ->
            "data-type-modifier"

        UseKeyword ->
            "use-keyword"

        UsePrefix ->
            "use-prefix"

        UseSuffix ->
            "use-suffix"

        HashQualifier _ ->
            "hash-qualifier"

        DelayForceChar ->
            "delay-force-char"

        DelimiterChar ->
            "delimeter-char"

        Parenthesis ->
            "parenthesis"

        LinkKeyword ->
            "link-keyword"

        DocDelimiter ->
            "doc-delimeter"

        DocKeyword ->
            "doc-keyword"


viewSegment : (Hash -> msg) -> SyntaxSegment -> Html msg
viewSegment toReferenceClickMsg (SyntaxSegment sType sText) =
    let
        hash =
            case sType of
                Reference h ->
                    Just h

                Referent h ->
                    Just h

                _ ->
                    Nothing

        className =
            syntaxTypeToClassName sType
    in
    case hash of
        Just h ->
            a [ class className, onClick (toReferenceClickMsg h) ] [ text sText ]

        Nothing ->
            span [ class className ] [ text sText ]


view : (Hash -> msg) -> Syntax -> Html msg
view toReferenceClickMsg (Syntax segments) =
    let
        renderedSegments =
            segments
                |> NEL.map (viewSegment toReferenceClickMsg)
                |> NEL.toList
    in
    span [ class "syntax" ] renderedSegments



-- JSON DECODE


simpleSyntaxTypeFromString : String -> SyntaxType
simpleSyntaxTypeFromString rawType =
    case rawType of
        "NumericLiteral" ->
            NumericLiteral

        "TextLiteral" ->
            TextLiteral

        "BytesLiteral" ->
            BytesLiteral

        "CharLiteral" ->
            CharLiteral

        "BooleanLiteral" ->
            BooleanLiteral

        "Blank" ->
            Blank

        "Var" ->
            Var

        "Constructor" ->
            Constructor

        "Request" ->
            Request

        "AbilityBraces" ->
            AbilityBraces

        "ControlKeyword" ->
            ControlKeyword

        "TypeOperator" ->
            TypeOperator

        "BindingEquals" ->
            BindingEquals

        "TypeAscriptionColon" ->
            TypeAscriptionColon

        "DataTypeKeyword" ->
            DataTypeKeyword

        "DataTypeParams" ->
            DataTypeParams

        "Unit" ->
            Unit

        "DataTypeModifier" ->
            DataTypeModifier

        "UseKeyword" ->
            UseKeyword

        "UsePrefix" ->
            UsePrefix

        "UseSuffix" ->
            UseSuffix

        "DelayForceChar" ->
            DelayForceChar

        "DelimiterChar" ->
            DelimiterChar

        "Parenthesis" ->
            Parenthesis

        "LinkKeyword" ->
            LinkKeyword

        "DocDelimiter" ->
            DocDelimiter

        "DocKeyword" ->
            DocKeyword

        _ ->
            Blank


decodeOp : Decode.Decoder SyntaxType
decodeOp =
    let
        decodeOpTag =
            at [ "annotation", "contents", "tag" ] Decode.string
    in
    Decode.map
        Op
        (Decode.oneOf
            [ when decodeOpTag ((==) "Cons") (Decode.succeed Cons)
            , when decodeOpTag ((==) "Snoc") (Decode.succeed Snoc)
            , when decodeOpTag ((==) "Concat") (Decode.succeed Concat)
            ]
        )


decodeHashQualifier : Decode.Decoder SyntaxType
decodeHashQualifier =
    let
        decodeHashQualifierTag =
            at [ "annotation", "contents", "tag" ] Decode.string

        decodeNameOnly =
            Decode.map NameOnly (at [ "annotation", "contents", "contents", "toText" ] Decode.string)

        decodeHashOnly =
            Decode.map HashOnly (at [ "annotation", "contents", "contents" ] Hash.decode)

        decodeHashQualified =
            Decode.map2
                HashQualified
                (at [ "annotation", "contents", "contents", "toText" ] (Decode.index 0 Decode.string))
                (at [ "annotation", "contents", "contents" ] (Decode.index 1 Hash.decode))
    in
    Decode.map
        HashQualifier
        (Decode.oneOf
            [ when decodeHashQualifierTag ((==) "NameOnly") decodeNameOnly
            , when decodeHashQualifierTag ((==) "HashOnly") decodeHashOnly
            , when decodeHashQualifierTag ((==) "HashQualified") decodeHashQualified
            ]
        )


decodeTag : Decode.Decoder String
decodeTag =
    Decode.oneOf
        [ at [ "annotation", "tag" ] Decode.string
        , Decode.succeed "Blank"
        ]


decodeSyntaxSegment : Decode.Decoder SyntaxSegment
decodeSyntaxSegment =
    let
        decodeReferent =
            Decode.map Referent (at [ "annotation", "contents" ] Hash.decode)

        decodeReference =
            Decode.map Reference (at [ "annotation", "contents" ] Hash.decode)
    in
    Decode.map2 SyntaxSegment
        (Decode.oneOf
            [ when decodeTag ((==) "Referent") decodeReferent
            , when decodeTag ((==) "Reference") decodeReference
            , when decodeTag ((==) "Op") decodeOp
            , when decodeTag ((==) "HashQualifier") decodeHashQualifier
            , decodeTag |> andThen (simpleSyntaxTypeFromString >> Decode.succeed)
            ]
        )
        (field "segment" Decode.string)


decode : Decode.Decoder Syntax
decode =
    Util.decodeNonEmptyList decodeSyntaxSegment |> andThen (Syntax >> Decode.succeed)
