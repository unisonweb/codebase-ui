module Definition exposing (..)

import Api
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import Html exposing (Html, a, code, div, h3, header, section, span, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (at, field)
import Json.Decode.Extra exposing (when)
import List.Extra as ListE
import List.Nonempty as NEL
import Source
    exposing
        ( TermSource(..)
        , TypeSignature(..)
        , TypeSource(..)
        , viewTermSource
        , viewTypeSource
        )
import String.Extra exposing (pluralize)
import String.Interpolate exposing (interpolate)
import Syntax
import UI
import UI.Icon as Icon
import Util



-- TYPES


type alias TypeDefinitionInfo =
    DefinitionInfo TypeSource


type alias TermDefinitionInfo =
    DefinitionInfo TermSource


type alias DefinitionInfo s =
    { name : String
    , namespace : Maybe String
    , otherNames : List FQN
    , source : s
    }


type Definition
    = Term Hash TermDefinitionInfo
    | Type Hash TypeDefinitionInfo


makeTypeDefinitionInfo :
    String
    -> NEL.Nonempty FQN
    -> TypeSource
    -> TypeDefinitionInfo
makeTypeDefinitionInfo name_ allFqns source =
    let
        ( namespace, otherNames ) =
            namespaceAndOtherNames name_ allFqns
    in
    DefinitionInfo name_ namespace otherNames source


makeTermDefinitionInfo :
    String
    -> NEL.Nonempty FQN
    -> TermSource
    -> TermDefinitionInfo
makeTermDefinitionInfo name_ allFqns source =
    let
        ( namespace, otherNames ) =
            namespaceAndOtherNames name_ allFqns
    in
    DefinitionInfo name_ namespace otherNames source



-- HELPERS


hash : Definition -> Hash
hash definition =
    case definition of
        Type h _ ->
            h

        Term h _ ->
            h


name : Definition -> String
name definition =
    case definition of
        Type _ info ->
            info.name

        Term _ info ->
            info.name


equals : Definition -> Definition -> Bool
equals a b =
    Hash.equals (hash a) (hash b)


namespaceAndOtherNames : String -> NEL.Nonempty FQN -> ( Maybe String, List FQN )
namespaceAndOtherNames suffixName fqns =
    let
        fqnWithin =
            fqns
                |> NEL.filter (FQN.isSuffixOf suffixName) (NEL.head fqns)
                |> NEL.head

        fqnsWithout =
            fqns
                |> NEL.toList
                |> ListE.filterNot (FQN.equals fqnWithin)
    in
    ( FQN.namespaceOf suffixName fqnWithin, fqnsWithout )



-- VIEW


viewError : msg -> Hash -> Bool -> Http.Error -> Html msg
viewError closeMsg hash_ isFocused err =
    viewClosableRow closeMsg
        hash_
        isFocused
        (h3 [] [ text "Error" ])
        (UI.errorMessage (Api.errorToString err))


viewLoading : Hash -> Bool -> Html msg
viewLoading hash_ isFocused =
    viewRow
        hash_
        isFocused
        [ UI.loadingPlaceholder ]
        (div [] [ code [] [ UI.loadingPlaceholder, UI.loadingPlaceholder ] ])


viewNames :
    { a | name : String, namespace : Maybe String, otherNames : List FQN }
    -> Html msg
viewNames info =
    let
        namespace =
            case info.namespace of
                Just ns ->
                    div [ class "namespace" ]
                        [ span [ class "separator in" ] [ text "in" ]
                        , text ns
                        ]

                Nothing ->
                    UI.nothing

        numOtherNames =
            List.length info.otherNames

        otherNames =
            if numOtherNames > 0 then
                let
                    otherNamesTooltipContent =
                        div [] (List.map (\n -> div [] [ text (FQN.toString n) ]) info.otherNames)

                    otherNamesLabel =
                        [ String.fromInt numOtherNames, pluralize "name" "names" numOtherNames ]
                            |> interpolate "{0} other {1}..."
                            |> text
                in
                div []
                    [ span [ class "separator" ] [ text "•" ]
                    , span [ class "other-names" ] [ UI.withTooltip otherNamesTooltipContent otherNamesLabel ]
                    ]

            else
                UI.nothing
    in
    div [ class "names" ]
        [ h3 [ class "name" ] [ text info.name ]
        , div [ class "info" ]
            [ namespace
            , otherNames
            ]
        ]


view : msg -> (Hash -> msg) -> Definition -> Bool -> Html msg
view closeMsg toOpenReferenceMsg definition isFocused =
    let
        viewDefinitionInfo hash_ info source =
            viewClosableRow
                closeMsg
                hash_
                isFocused
                (viewNames info)
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
        [ classList [ ( "focused", isFocused ), ( "definition-row", True ) ]
        , id ("definition-" ++ Hash.toString hash_)
        ]
        [ header [] (indicator :: headerItems)
        , section [ class "content" ] [ content ]
        ]


viewClosableRow : msg -> Hash -> Bool -> Html msg -> Html msg -> Html msg
viewClosableRow closeMsg hash_ isFocused header content =
    let
        close =
            a [ class "close", onClick closeMsg ] [ Icon.view Icon.X ]
    in
    viewRow hash_ isFocused [ header, close ] content



-- JSON DECODERS


decodeTypeDefInfo : Decode.Decoder TypeDefinitionInfo
decodeTypeDefInfo =
    let
        decodeTypeDefTag =
            at [ "typeDefinition", "tag" ] Decode.string

        decodeUserObject =
            Decode.map TypeSource (at [ "typeDefinition", "contents" ] Syntax.decode)
    in
    Decode.map3 makeTypeDefinitionInfo
        (field "bestTypeName" Decode.string)
        (field "typeNames" (Util.decodeNonEmptyList FQN.decode))
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
    Decode.map3 makeTermDefinitionInfo
        (field "bestTermName" Decode.string)
        (field "termNames" (Util.decodeNonEmptyList FQN.decode))
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
