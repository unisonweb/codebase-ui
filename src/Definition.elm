module Definition exposing (..)

import Api
import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Html exposing (Html, a, code, div, h3, header, section, span, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (at, field)
import Json.Decode.Extra exposing (when)
import List.Nonempty as NEL
import Source
    exposing
        ( TermSource(..)
        , TypeSignature(..)
        , TypeSource(..)
        , viewTermSource
        , viewTypeSource
        )
import Syntax
import UI
import UI.Icon as Icon
import Util



-- TYPES


type alias TypeDefinitionInfo =
    { fqns : NEL.Nonempty FQN
    , name : String
    , source : TypeSource
    }


type alias TermDefinitionInfo =
    { fqns : NEL.Nonempty FQN
    , name : String
    , source : TermSource
    }


type Definition
    = Term Hash TermDefinitionInfo
    | Type Hash TypeDefinitionInfo



-- HELPERS


hash : Definition -> Hash
hash definition =
    case definition of
        Type h _ ->
            h

        Term h _ ->
            h



-- VIEW


viewError : msg -> Hash -> Bool -> Http.Error -> Html msg
viewError closeMsg hash_ isFocused err =
    viewClosableRow closeMsg
        hash_
        isFocused
        (text "Error")
        (UI.errorMessage (Api.errorToString err))


viewLoading : Hash -> Bool -> Html msg
viewLoading hash_ isFocused =
    viewRow
        hash_
        isFocused
        [ UI.loadingPlaceholder ]
        (div [] [ code [] [ UI.loadingPlaceholder, UI.loadingPlaceholder ] ])


view : msg -> (Hash -> msg) -> Definition -> Bool -> Html msg
view closeMsg toOpenReferenceMsg definition isFocused =
    let
        viewDefinitionInfo hash_ info source =
            viewClosableRow
                closeMsg
                hash_
                isFocused
                (text info.name)
                (div [] [ source ])
    in
    case definition of
        Term h info ->
            viewDefinitionInfo h
                info
                (viewTermSource toOpenReferenceMsg info.name info.source)

        Type h info ->
            viewDefinitionInfo h
                info
                (viewTypeSource toOpenReferenceMsg info.source)



-- VIEW HELPERS


viewRow : Hash -> Bool -> List (Html msg) -> Html msg -> Html msg
viewRow hash_ isFocused headerItems content =
    let
        indicator : Html msg
        indicator =
            span [ class "focus-indicator" ] []
    in
    div
        [ classList [ ( "focus", isFocused ), ( "definition-row", True ) ]
        , id
            ("definition-" ++ Hash.toString hash_)
        ]
        [ header [] (indicator :: headerItems)
        , section
            [ class "content"
            ]
            [ content ]
        ]


viewClosableRow : msg -> Hash -> Bool -> Html msg -> Html msg -> Html msg
viewClosableRow closeMsg hash_ isFocused title content =
    viewRow
        hash_
        isFocused
        [ h3 [] [ title ]
        , a [ class "close", onClick closeMsg ] [ Icon.view Icon.X ]
        ]
        content



-- JSON DECODERS


decodeTypeDefInfo : Decode.Decoder TypeDefinitionInfo
decodeTypeDefInfo =
    let
        decodeTypeDefTag =
            at [ "typeDefinition", "tag" ] Decode.string

        decodeUserObject =
            Decode.map TypeSource (at [ "typeDefinition", "contents" ] Syntax.decode)
    in
    Decode.map3 TypeDefinitionInfo
        (field "typeNames" (Util.decodeNonEmptyList FullyQualifiedName.decode))
        (field "bestTypeName" Decode.string)
        (Decode.oneOf
            [ when decodeTypeDefTag ((==) "UserObject") decodeUserObject
            , when decodeTypeDefTag ((==) "BuiltinObject") (Decode.succeed BuiltinType)
            ]
        )


decodeTypes : Decode.Decoder (List Definition)
decodeTypes =
    let
        buildTypes =
            List.map (\( h, d ) -> Type (Hash.fromString h) d)
    in
    Decode.keyValuePairs decodeTypeDefInfo |> Decode.map buildTypes


decodeTermDefInfo : Decode.Decoder TermDefinitionInfo
decodeTermDefInfo =
    let
        decodeTermDefTag =
            at [ "termDefinition", "tag" ] Decode.string

        decodeUserObject =
            Decode.map TermSource (at [ "termDefinition", "contents" ] Syntax.decode)

        decodeBuiltin =
            Decode.map (TypeSignature >> BuiltinTerm) (field "signature" Syntax.decode)
    in
    Decode.map3 TermDefinitionInfo
        (field "termNames" (Util.decodeNonEmptyList FullyQualifiedName.decode))
        (field "bestTermName" Decode.string)
        (Decode.oneOf
            [ when decodeTermDefTag ((==) "UserObject") decodeUserObject
            , when decodeTermDefTag ((==) "BuiltinObject") decodeBuiltin
            ]
        )


decodeTerms : Decode.Decoder (List Definition)
decodeTerms =
    let
        buildTerms =
            List.map (\( h, d ) -> Term (Hash.fromString h) d)
    in
    Decode.keyValuePairs decodeTermDefInfo |> Decode.map buildTerms


decodeList : Decode.Decoder (List Definition)
decodeList =
    Decode.map2 List.append
        (field "termDefinitions" decodeTerms)
        (field "typeDefinitions" decodeTypes)


decodeHead : Decode.Decoder Definition
decodeHead =
    Decode.map List.head decodeList
        |> Decode.andThen
            (Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Empty list")
            )
