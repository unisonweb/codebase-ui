module Code.Syntax exposing
    ( Linked(..)
    , Syntax
    , SyntaxSegment(..)
    , SyntaxType(..)
    , Width(..)
    , decode
    , decodeSingleton
    , foldl
    , numLines
    , reference
    , view
    )

import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.HashQualified as HQ
import Html exposing (Html, a, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (andThen, at, field)
import Json.Decode.Extra exposing (when)
import Lib.Util as Util
import List.Nonempty as NEL


type Width
    = Width Int


type Syntax
    = Syntax (NEL.Nonempty SyntaxSegment)


type SyntaxSegment
    = SyntaxSegment SyntaxType String


type SeqOp
    = Cons
    | Snoc
    | Concat


type SyntaxType
    = NumericLiteral
    | TextLiteral
    | BytesLiteral
    | CharLiteral
    | BooleanLiteral
    | Blank
    | Var
    | TypeReference Hash
    | TermReference Hash
      -- +:|:+|++
    | Op SeqOp
    | DataConstructorReference Hash
    | AbilityConstructorReference Hash
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
      -- TODO: This should be a HashQualified
    | HashQualifier String
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


type Linked msg
    = Linked (Reference -> msg)
    | NotLinked



-- HELPERS


{-| TODO: Parse Syntax into a list of lines and this function can be removed
-}
numLines : Syntax -> Int
numLines (Syntax segments) =
    let
        count (SyntaxSegment _ segment) acc =
            if String.contains "\n" segment then
                acc + 1

            else
                acc
    in
    NEL.foldl count 1 segments


reference : SyntaxSegment -> Maybe Reference
reference (SyntaxSegment syntaxType _) =
    case syntaxType of
        TypeReference h ->
            Just (Reference.TypeReference (HQ.HashOnly h))

        TermReference h ->
            Just (Reference.TermReference (HQ.HashOnly h))

        AbilityConstructorReference h ->
            Just (Reference.AbilityConstructorReference (HQ.HashOnly h))

        DataConstructorReference h ->
            Just (Reference.DataConstructorReference (HQ.HashOnly h))

        _ ->
            Nothing


foldl : (SyntaxSegment -> b -> b) -> b -> Syntax -> b
foldl f init (Syntax segments) =
    NEL.foldl f init segments



-- VIEW


syntaxTypeToClassName : SyntaxType -> String
syntaxTypeToClassName sType =
    case sType of
        NumericLiteral ->
            "numeric-literal"

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

        TypeReference _ ->
            "type-reference"

        TermReference _ ->
            "term-reference"

        DataConstructorReference _ ->
            "data-constructor-reference"

        AbilityConstructorReference _ ->
            "ability-constructor-reference"

        Op seqOp ->
            case seqOp of
                Cons ->
                    "op cons"

                Snoc ->
                    "op snoc"

                Concat ->
                    "op concat"

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


viewFQN : FQN -> Html msg
viewFQN fqn =
    fqn
        |> FQN.segments
        |> NEL.map (\s -> span [ class "segment" ] [ text s ])
        |> NEL.toList
        |> List.intersperse (span [ class "sep" ] [ text "." ])
        |> span [ class "fqn" ]


viewSegment : Linked msg -> SyntaxSegment -> Html msg
viewSegment linked (SyntaxSegment sType sText) =
    let
        ref =
            case sType of
                TypeReference h ->
                    Just (Reference.TypeReference (HQ.HashOnly h))

                TermReference h ->
                    Just (Reference.TermReference (HQ.HashOnly h))

                AbilityConstructorReference h ->
                    Just (Reference.AbilityConstructorReference (HQ.HashOnly h))

                DataConstructorReference h ->
                    Just (Reference.DataConstructorReference (HQ.HashOnly h))

                _ ->
                    Nothing

        isFQN =
            let
                isFQN_ =
                    String.contains "." sText
            in
            case sType of
                TypeReference _ ->
                    isFQN_

                TermReference _ ->
                    isFQN_

                HashQualifier _ ->
                    isFQN_

                DataConstructorReference _ ->
                    isFQN_

                AbilityConstructorReference _ ->
                    isFQN_

                _ ->
                    False

        className =
            syntaxTypeToClassName sType

        content =
            if String.contains "->" sText then
                span [ class "arrow" ] [ text sText ]

            else if isFQN then
                viewFQN (FQN.fromString sText)

            else
                text sText
    in
    case ( linked, ref ) of
        ( Linked toReferenceClickMsg, Just r ) ->
            a [ class className, onClick (toReferenceClickMsg r) ] [ content ]

        _ ->
            span [ class className ] [ content ]


view : Linked msg -> Syntax -> Html msg
view linked (Syntax segments) =
    let
        renderedSegments =
            segments
                |> NEL.map (viewSegment linked)
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


decodeTag : Decode.Decoder String
decodeTag =
    Decode.oneOf
        [ at [ "annotation", "tag" ] Decode.string
        , Decode.succeed "Blank"
        ]


decodeSyntaxSegment : Decode.Decoder SyntaxSegment
decodeSyntaxSegment =
    let
        hashToReference hash =
            if Hash.isDataConstructorHash hash then
                DataConstructorReference hash

            else if Hash.isAbilityConstructorHash hash then
                AbilityConstructorReference hash

            else
                TermReference hash

        decodeReference =
            Decode.map hashToReference (at [ "annotation", "contents" ] Hash.decode)

        decodeTypeReference =
            Decode.map TypeReference (at [ "annotation", "contents" ] Hash.decode)

        decodeHashQualifier =
            Decode.map HashQualifier (at [ "annotation", "contents" ] Decode.string)
    in
    Decode.map2 SyntaxSegment
        (Decode.oneOf
            [ when decodeTag ((==) "TermReference") decodeReference
            , when decodeTag ((==) "TypeReference") decodeTypeReference
            , when decodeTag ((==) "Op") decodeOp
            , when decodeTag ((==) "HashQualifier") decodeHashQualifier
            , decodeTag |> andThen (simpleSyntaxTypeFromString >> Decode.succeed)
            ]
        )
        (field "segment" Decode.string)


decodeSingleton : Decode.Decoder Syntax
decodeSingleton =
    Decode.map Syntax (Decode.map NEL.fromElement decodeSyntaxSegment)


decode : Decode.Decoder Syntax
decode =
    Util.decodeNonEmptyList decodeSyntaxSegment |> andThen (Syntax >> Decode.succeed)
