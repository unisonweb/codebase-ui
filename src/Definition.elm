-- TODO: DEPRECATE


module Definition exposing (..)

import Api
import Definition.Category as Category exposing (Category)
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified exposing (HashQualified(..))
import Html exposing (Html, a, code, div, h3, header, section, span, strong, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (at, field)
import Json.Decode.Extra exposing (when)
import List.Extra as ListE
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import Source
    exposing
        ( TermSource(..)
        , TypeSignature(..)
        , TypeSource(..)
        , viewTermSource
        , viewTypeSource
        )
import String.Extra exposing (pluralize)
import Syntax
import UI
import UI.Icon as Icon
import Util
import Workspace.Reference exposing (Reference(..))



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


reference : Definition -> Reference
reference definition =
    case definition of
        Type h _ ->
            TypeReference (HashOnly h)

        Term h _ ->
            TermReference (HashOnly h)


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


viewBuiltinBadge : String -> Category -> Html msg
viewBuiltinBadge name_ category =
    let
        content =
            span
                []
                [ strong [] [ text name_ ]
                , text " is a "
                , strong [] [ text ("built-in " ++ Category.name category) ]
                , text " provided by the Unison runtime"
                ]
    in
    UI.badge content


viewBuiltin : Definition -> Html msg
viewBuiltin definition =
    case definition of
        Term _ info ->
            case info.source of
                BuiltinTerm _ ->
                    div [ class "built-in" ] [ viewBuiltinBadge info.name (Category.Term Category.PlainTerm) ]

                TermSource _ _ ->
                    UI.nothing

        Type _ info ->
            case info.source of
                BuiltinType ->
                    div [ class "built-in" ] [ viewBuiltinBadge info.name (Category.Type Category.DataType) ]

                TypeSource _ ->
                    UI.nothing


viewError : msg -> Hash -> Bool -> Http.Error -> Html msg
viewError closeMsg hash_ isFocused err =
    viewClosableRow closeMsg
        hash_
        isFocused
        (h3 [] [ text "Error" ])
        [ ( UI.nothing, UI.errorMessage (Api.errorToString err) ) ]


viewLoading : Hash -> Bool -> Html msg
viewLoading hash_ isFocused =
    viewDefinitionRow
        hash_
        isFocused
        ( UI.nothing, UI.loadingPlaceholder )
        [ ( UI.nothing, div [] [ code [] [ UI.loadingPlaceholder ] ] ) ]


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
                        text (pluralize "other name..." "other names..." numOtherNames)
                in
                div []
                    [ span [ class "separator" ] [ text "•" ]
                    , span [ class "other-names" ] [ UI.withTooltip otherNamesTooltipContent otherNamesLabel ]
                    ]

            else
                UI.nothing
    in
    div [ class "names" ]
        [ Icon.view Icon.CaretDown
        , Icon.view Icon.Type
        , h3 [ class "name" ] [ text info.name ]
        , div [ class "info" ] [ namespace, otherNames ]
        ]


viewSource : (Hash -> msg) -> Definition -> ( Html msg, Html msg )
viewSource toOpenReferenceMsg definition =
    let
        sourceConfig =
            Source.Rich toOpenReferenceMsg

        viewLineGutter numLines =
            let
                lines =
                    numLines
                        |> List.range 1
                        |> List.map (String.fromInt >> text >> List.singleton >> div [])
            in
            UI.codeBlock [] (div [] lines)

        viewToggableSource icon disabled renderedSource =
            div [ class "source" ]
                [ div
                    [ classList
                        [ ( "source-toggle", True )
                        , ( "disabled", disabled )
                        ]
                    ]
                    [ Icon.view icon ]
                , renderedSource
                ]
    in
    case definition of
        Term _ info ->
            ( info.source, info.source )
                |> Tuple.mapBoth Source.numTermLines (viewTermSource sourceConfig info.name)
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.CaretDown False)

        Type _ info ->
            ( info.source, info.source )
                |> Tuple.mapBoth Source.numTypeLines (viewTypeSource sourceConfig)
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.CaretRight True)


view : msg -> (Hash -> msg) -> Definition -> Bool -> Html msg
view closeMsg toOpenReferenceMsg definition isFocused =
    let
        viewDefinitionInfo hash_ info =
            viewClosableRow
                closeMsg
                hash_
                isFocused
                (viewNames info)
                [ ( UI.nothing, viewBuiltin definition )
                , viewSource toOpenReferenceMsg definition
                ]
    in
    case definition of
        Term h info ->
            viewDefinitionInfo h info

        Type h info ->
            viewDefinitionInfo h info



-- VIEW HELPERS


viewGutter : Html msg -> Html msg
viewGutter content =
    div [ class "gutter" ] [ content ]


viewDefinitionRow :
    Hash
    -> Bool
    -> ( Html msg, Html msg )
    -> List ( Html msg, Html msg )
    -> Html msg
viewDefinitionRow hash_ isFocused ( headerGutter, headerContent ) content =
    let
        headerItems =
            [ viewGutter headerGutter, headerContent ]

        contentRows =
            List.map (\( g, c ) -> div [ class "inner-row" ] [ viewGutter g, c ]) content
    in
    div
        [ classList [ ( "focused", isFocused ), ( "definition-row", True ) ]
        , id ("definition-" ++ Hash.toString hash_)
        ]
        [ header [ class "inner-row" ] headerItems
        , section [ class "content" ] contentRows
        ]


viewClosableRow :
    msg
    -> Hash
    -> Bool
    -> Html msg
    -> List ( Html msg, Html msg )
    -> Html msg
viewClosableRow closeMsg hash_ isFocused header contentItems =
    let
        close =
            a [ class "close", onClick closeMsg ] [ Icon.view Icon.X ]
    in
    viewDefinitionRow hash_ isFocused ( close, header ) contentItems



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
            List.map (\( h, d ) -> Maybe.map (\h_ -> Type h_ d) (Hash.fromString h))
                >> MaybeE.values
    in
    Decode.keyValuePairs decodeTypeDefInfo |> Decode.map buildTypes


decodeTermDefInfo : Decode.Decoder TermDefinitionInfo
decodeTermDefInfo =
    let
        decodeTermDefTag =
            at [ "termDefinition", "tag" ] Decode.string

        decodeUserObject =
            Decode.map2 TermSource
                (Decode.map TypeSignature (field "signature" Syntax.decode))
                (at [ "termDefinition", "contents" ] Syntax.decode)

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
            List.map (\( h, d ) -> Maybe.map (\h_ -> Term h_ d) (Hash.fromString h))
                >> MaybeE.values
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
