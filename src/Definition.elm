module Definition exposing (..)

import Api
import FullyQualifiedName exposing (FQN)
import Hash exposing (Hash)
import Html exposing (Html, a, article, aside, button, code, div, h1, h2, h3, header, input, label, nav, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (andThen, at, field)
import List.Nonempty as NEL
import Source exposing (TermSource(..), TypeSignature(..), TypeSource(..), viewTermSource, viewTypeSource)
import Syntax
import UI
import UI.Icon
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


viewDefinitionRow : List (Html msg) -> Html msg -> Html msg
viewDefinitionRow headerItems content =
    div [ class "definition-row" ]
        [ header [] headerItems, section [ class "content" ] [ content ] ]


viewClosableRow : msg -> Html msg -> Html msg -> Html msg
viewClosableRow closeMsg title content =
    viewDefinitionRow
        [ h3 [] [ title ]
        , a [ class "close", onClick closeMsg ] [ UI.Icon.x ]
        ]
        content


viewError : msg -> Http.Error -> Html msg
viewError closeMsg err =
    viewClosableRow closeMsg (text "Error") (UI.errorMessage (Api.errorToString err))


viewLoading : Html msg
viewLoading =
    viewDefinitionRow [ UI.loadingPlaceholder ]
        (div
            []
            [ div [ class "docs" ] [ UI.loadingPlaceholder ]
            , code [] [ UI.loadingPlaceholder, UI.loadingPlaceholder ]
            ]
        )


view : msg -> Definition -> Html msg
view closeMsg definition =
    let
        viewDefinitionInfo info source =
            viewClosableRow
                closeMsg
                (div [] [ text info.name ])
                (div [] [ source ])
    in
    case definition of
        Term _ info ->
            viewDefinitionInfo info (viewTermSource info.name info.source)

        Type _ info ->
            viewDefinitionInfo info (viewTypeSource info.source)



-- JSON DECODERS


decodeTypeDefInfo : Decode.Decoder TypeDefinitionInfo
decodeTypeDefInfo =
    Decode.map3 TypeDefinitionInfo
        (field "typeNames" (Util.decodeNonEmptyList FullyQualifiedName.decode))
        (field "bestTypeName" Decode.string)
        (Decode.oneOf
            [ field "typeDefinition" Syntax.decode |> andThen (TypeSource >> Decode.succeed)
            , Decode.succeed BuiltinType
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
    Decode.map3 TermDefinitionInfo
        (field "termNames" (Util.decodeNonEmptyList FullyQualifiedName.decode))
        (field "bestTermName" Decode.string)
        (Decode.oneOf
            [ Decode.map TermSource (at [ "termDefinition", "contents" ] Syntax.decode)
            , Decode.map (TypeSignature >> BuiltinTerm) (field "signature" Syntax.decode)
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
