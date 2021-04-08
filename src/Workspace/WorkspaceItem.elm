module Workspace.WorkspaceItem exposing (..)

import Api
import Definition.Category as Category exposing (Category)
import Definition.Info as Info
import Definition.Source as Source
import Definition.Term as Term exposing (Term(..), TermDetail, TermSignature(..), TermSource(..))
import Definition.Type as Type exposing (Type(..), TypeDetail, TypeSource(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash
import HashQualified exposing (HashQualified(..))
import Html exposing (Html, a, div, h3, header, section, span, strong, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (at, field)
import Json.Decode.Extra exposing (when)
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import String.Extra exposing (pluralize)
import Syntax
import UI
import UI.Icon as Icon
import Util
import Workspace.Reference as Reference exposing (Reference(..))


type WorkspaceItem
    = Loading Reference
    | Failure Reference Http.Error
    | Success Reference Item


type Item
    = TermItem TermDetail
    | TypeItem TypeDetail


reference : WorkspaceItem -> Reference
reference item =
    case item of
        Loading r ->
            r

        Failure r _ ->
            r

        Success r _ ->
            r


isSameReference : WorkspaceItem -> Reference -> Bool
isSameReference item ref =
    reference item == ref


isSameByReference : WorkspaceItem -> WorkspaceItem -> Bool
isSameByReference a b =
    reference a == reference b



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


viewBuiltin : Item -> Html msg
viewBuiltin item =
    case item of
        TermItem (Term info source) ->
            case source of
                Term.Builtin _ ->
                    div [ class "built-in" ]
                        [ viewBuiltinBadge info.name (Category.Term Category.PlainTerm) ]

                Term.Source _ _ ->
                    UI.nothing

        TypeItem (Type info source) ->
            case source of
                Type.Builtin ->
                    div [ class "built-in" ]
                        [ viewBuiltinBadge info.name (Category.Type Category.DataType) ]

                Type.Source _ ->
                    UI.nothing


{-| TODO: Some of this that isn't Workspace specific might be moved into Definition.Info
-}
viewNames :
    { a | name : String, namespace : Maybe String, otherNames : List FQN }
    -> Category
    -> Html msg
viewNames info category =
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
        , Icon.view (Category.icon category)
        , h3 [ class "name" ] [ text info.name ]
        , div [ class "info" ] [ namespace, otherNames ]
        ]


{-| TODO: Yikes, this isn't great. Needs cleanup
-}
viewSource : (Reference -> msg) -> Item -> ( Html msg, Html msg )
viewSource toOpenReferenceMsg item =
    let
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
    case item of
        TermItem (Term info source) ->
            ( source, source )
                |> Tuple.mapBoth Source.numTermLines (Source.viewTermSource (Source.Rich (HashOnly >> TermReference >> toOpenReferenceMsg)) info.name)
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.CaretDown False)

        TypeItem (Type _ source) ->
            ( source, source )
                |> Tuple.mapBoth Source.numTypeLines (Source.viewTypeSource (Source.Rich (HashOnly >> TypeReference >> toOpenReferenceMsg)))
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.CaretRight True)


viewItem : msg -> (Reference -> msg) -> Reference -> Item -> Bool -> Html msg
viewItem closeMsg toOpenReferenceMsg ref item isFocused =
    case item of
        TermItem (Term info _) ->
            viewClosableRow
                closeMsg
                ref
                isFocused
                (viewNames info (Category.Term Category.PlainTerm))
                [ ( UI.nothing, viewBuiltin item )
                , viewSource toOpenReferenceMsg item
                ]

        TypeItem (Type info _) ->
            viewClosableRow
                closeMsg
                ref
                isFocused
                (viewNames info (Category.Type Category.DataType))
                [ ( UI.nothing, viewBuiltin item )
                , viewSource toOpenReferenceMsg item
                ]


view : msg -> (Reference -> msg) -> WorkspaceItem -> Bool -> Html msg
view closeMsg toOpenReferenceMsg workspaceItem isFocused =
    case workspaceItem of
        Loading ref ->
            viewRow ref isFocused ( UI.nothing, UI.loadingPlaceholder ) [ ( UI.nothing, UI.loadingPlaceholder ) ]

        Failure ref err ->
            viewClosableRow
                closeMsg
                ref
                isFocused
                (h3 [] [ text "Error" ])
                [ ( UI.nothing, UI.errorMessage (Api.errorToString err) ) ]

        Success ref i ->
            viewItem closeMsg toOpenReferenceMsg ref i isFocused



-- VIEW HELPERS


viewGutter : Html msg -> Html msg
viewGutter content =
    div [ class "gutter" ] [ content ]


viewRow :
    Reference
    -> Bool
    -> ( Html msg, Html msg )
    -> List ( Html msg, Html msg )
    -> Html msg
viewRow ref isFocused ( headerGutter, headerContent ) content =
    let
        headerItems =
            [ viewGutter headerGutter, headerContent ]

        contentRows =
            List.map (\( g, c ) -> div [ class "inner-row" ] [ viewGutter g, c ]) content
    in
    div
        [ classList [ ( "focused", isFocused ), ( "definition-row", True ) ]
        , id ("definition-" ++ Reference.toString ref)
        ]
        [ header [ class "inner-row" ] headerItems
        , section [ class "content" ] contentRows
        ]


viewClosableRow :
    msg
    -> Reference
    -> Bool
    -> Html msg
    -> List ( Html msg, Html msg )
    -> Html msg
viewClosableRow closeMsg hash_ isFocused header contentItems =
    let
        close =
            a [ class "close", onClick closeMsg ] [ Icon.view Icon.X ]
    in
    viewRow hash_ isFocused ( close, header ) contentItems



-- JSON DECODERS


decodeTypeNamesAndSource :
    Decode.Decoder
        { name : String
        , otherNames : NEL.Nonempty FQN
        , source : TypeSource
        }
decodeTypeNamesAndSource =
    let
        make name otherNames source =
            { name = name, otherNames = otherNames, source = source }

        decodeTypeDefTag =
            at [ "typeDefinition", "tag" ] Decode.string

        decodeUserObject =
            Decode.map Type.Source (at [ "typeDefinition", "contents" ] Syntax.decode)
    in
    Decode.map3 make
        (field "bestTypeName" Decode.string)
        (field "typeNames" (Util.decodeNonEmptyList FQN.decode))
        (Decode.oneOf
            [ when decodeTypeDefTag ((==) "UserObject") decodeUserObject
            , when decodeTypeDefTag ((==) "BuiltinObject") (Decode.succeed Type.Builtin)
            ]
        )


decodeTypes : Decode.Decoder (List TypeDetail)
decodeTypes =
    let
        makeType ( hash_, i ) =
            hash_
                |> Hash.fromString
                |> Maybe.map (\h -> Type (Info.makeInfo h i.name i.otherNames) i.source)

        buildTypes =
            List.map makeType >> MaybeE.values
    in
    Decode.keyValuePairs decodeTypeNamesAndSource |> Decode.map buildTypes


decodeTermNamesAndSource :
    Decode.Decoder
        { name : String
        , otherNames : NEL.Nonempty FQN
        , source : TermSource
        }
decodeTermNamesAndSource =
    let
        make name otherNames source =
            { name = name
            , otherNames = otherNames
            , source = source
            }

        decodeTermDefTag =
            at [ "termDefinition", "tag" ] Decode.string

        decodeUserObject =
            Decode.map2 Term.Source
                (Decode.map TermSignature (field "signature" Syntax.decode))
                (at [ "termDefinition", "contents" ] Syntax.decode)

        decodeBuiltin =
            Decode.map (TermSignature >> Term.Builtin) (field "signature" Syntax.decode)
    in
    Decode.map3 make
        (field "bestTermName" Decode.string)
        (field "termNames" (Util.decodeNonEmptyList FQN.decode))
        (Decode.oneOf
            [ when decodeTermDefTag ((==) "UserObject") decodeUserObject
            , when decodeTermDefTag ((==) "BuiltinObject") decodeBuiltin
            ]
        )


decodeTerms : Decode.Decoder (List TermDetail)
decodeTerms =
    let
        makeTerm ( hash_, i ) =
            hash_
                |> Hash.fromString
                |> Maybe.map (\h -> Term (Info.makeInfo h i.name i.otherNames) i.source)

        buildTerms =
            List.map makeTerm >> MaybeE.values
    in
    Decode.keyValuePairs decodeTermNamesAndSource |> Decode.map buildTerms


decodeList : Decode.Decoder (List Item)
decodeList =
    Decode.map2 List.append
        (Decode.map (List.map TermItem) (field "termDefinitions" decodeTerms))
        (Decode.map (List.map TypeItem) (field "typeDefinitions" decodeTypes))


decodeItem : Decode.Decoder Item
decodeItem =
    Decode.map List.head decodeList
        |> Decode.andThen
            (Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Empty list")
            )
