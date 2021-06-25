module Workspace.WorkspaceItem exposing (..)

import Api
import Definition.AbilityConstructor exposing (AbilityConstructor(..), AbilityConstructorDetail, AbilityConstructorSource(..))
import Definition.Category as Category exposing (Category)
import Definition.DataConstructor exposing (DataConstructor(..), DataConstructorDetail, DataConstructorSource(..))
import Definition.Doc as Doc exposing (Doc(..), DocFoldToggles)
import Definition.Info as Info
import Definition.Reference as Reference exposing (Reference(..))
import Definition.Source as Source
import Definition.Term as Term exposing (Term(..), TermCategory, TermDetail, TermSignature(..), TermSource(..))
import Definition.Type as Type exposing (Type(..), TypeCategory, TypeDetail, TypeSource(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash
import HashQualified as HQ exposing (HashQualified(..))
import Html exposing (Attribute, Html, a, div, h3, header, section, span, strong, text)
import Html.Attributes exposing (class, classList, id, title)
import Html.Events exposing (onClick)
import Http
import Id exposing (Id)
import Json.Decode as Decode exposing (at, field)
import Json.Decode.Extra exposing (when)
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import String.Extra exposing (pluralize)
import Syntax
import UI
import UI.Icon as Icon
import Util
import Workspace.Zoom exposing (Zoom(..))


type WorkspaceItem
    = Loading Reference
    | Failure Reference Http.Error
    | Success
        Reference
        { item : Item
        , zoom : Zoom
        , docFoldToggles : DocFoldToggles
        }


type alias TermDetailWithDoc =
    TermDetail { doc : Maybe Doc }


type alias TypeDetailWithDoc =
    TypeDetail { doc : Maybe Doc }


type Item
    = TermItem TermDetailWithDoc
    | TypeItem TypeDetailWithDoc
    | DataConstructorItem DataConstructorDetail
    | AbilityConstructorItem AbilityConstructorDetail


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
        TermItem (Term _ category detail) ->
            case detail.source of
                Term.Builtin _ ->
                    div [ class "built-in" ]
                        [ viewBuiltinBadge detail.info.name (Category.Term category) ]

                Term.Source _ _ ->
                    UI.nothing

        TypeItem (Type _ category detail) ->
            case detail.source of
                Type.Builtin ->
                    div [ class "built-in" ]
                        [ viewBuiltinBadge detail.info.name (Category.Type category) ]

                Type.Source _ ->
                    UI.nothing

        DataConstructorItem (DataConstructor _ detail) ->
            case detail.source of
                Type.Builtin ->
                    div [ class "built-in" ]
                        [ viewBuiltinBadge detail.info.name (Category.Type Type.DataType) ]

                Type.Source _ ->
                    UI.nothing

        AbilityConstructorItem (AbilityConstructor _ detail) ->
            case detail.source of
                Type.Builtin ->
                    div [ class "built-in" ]
                        [ viewBuiltinBadge detail.info.name (Category.Type Type.AbilityType) ]

                Type.Source _ ->
                    UI.nothing


{-| TODO: Some of this that isn't Workspace specific might be moved into Definition.Info
-}
viewNames :
    msg
    -> { a | name : String, namespace : Maybe String, otherNames : List FQN }
    -> Category
    -> Html msg
viewNames onClick_ info category =
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
    div [ class "names", onClick onClick_ ]
        [ Icon.view Icon.caretRight
        , Icon.view (Category.icon category)
        , h3 [ class "name" ] [ text info.name ]
        , div [ class "info" ] [ namespace, otherNames ]
        ]


viewDoc : (Reference -> msg) -> (Id Doc -> msg) -> DocFoldToggles -> Doc -> Html msg
viewDoc toOpenReferenceMsg toggleFoldMsg docFoldToggles doc =
    div [ class "definition-doc" ] [ Doc.view toOpenReferenceMsg toggleFoldMsg docFoldToggles doc ]


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
                    [ class "source-toggle"
                    , classList [ ( "disabled", disabled ) ]
                    ]
                    [ Icon.view icon ]
                , renderedSource
                ]
    in
    case item of
        TermItem (Term _ _ detail) ->
            ( detail.source, detail.source )
                |> Tuple.mapBoth Source.numTermLines (Source.viewTermSource (Source.Rich toOpenReferenceMsg) detail.info.name)
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.caretRight False)

        TypeItem (Type _ _ detail) ->
            ( detail.source, detail.source )
                |> Tuple.mapBoth Source.numTypeLines (Source.viewTypeSource (Source.Rich toOpenReferenceMsg))
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.caretRight True)

        DataConstructorItem (DataConstructor _ detail) ->
            ( detail.source, detail.source )
                |> Tuple.mapBoth Source.numTypeLines (Source.viewTypeSource (Source.Rich toOpenReferenceMsg))
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.caretRight True)

        AbilityConstructorItem (AbilityConstructor _ detail) ->
            ( detail.source, detail.source )
                |> Tuple.mapBoth Source.numTypeLines (Source.viewTypeSource (Source.Rich toOpenReferenceMsg))
                |> Tuple.mapBoth viewLineGutter (viewToggableSource Icon.caretRight True)


viewItem :
    msg
    -> (Reference -> msg)
    -> (Zoom -> msg)
    -> (Id Doc -> msg)
    -> Reference
    -> { item : Item, zoom : Zoom, docFoldToggles : DocFoldToggles }
    -> Bool
    -> Html msg
viewItem closeMsg toOpenReferenceMsg toUpdateZoomMsg toggleFoldMsg ref data isFocused =
    let
        -- TODO: Support zoom level on the source
        ( zoomClass, docZoomMsg, _ ) =
            case data.zoom of
                Far ->
                    ( "zoom-level-far", toUpdateZoomMsg Medium, toUpdateZoomMsg Near )

                Medium ->
                    ( "zoom-level-medium", toUpdateZoomMsg Far, toUpdateZoomMsg Near )

                Near ->
                    ( "zoom-level-near", toUpdateZoomMsg Far, toUpdateZoomMsg Near )

        attrs =
            [ class zoomClass, classList [ ( "focused", isFocused ) ] ]

        viewDoc_ doc =
            doc
                |> Maybe.map (viewDoc toOpenReferenceMsg toggleFoldMsg data.docFoldToggles)
                |> Maybe.withDefault UI.nothing
    in
    case data.item of
        TermItem (Term _ category detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewNames docZoomMsg detail.info (Category.Term category))
                [ ( UI.nothing, viewDoc_ detail.doc )
                , ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]

        TypeItem (Type _ category detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewNames docZoomMsg detail.info (Category.Type category))
                [ ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]

        DataConstructorItem (DataConstructor _ detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewNames docZoomMsg detail.info (Category.Type Type.DataType))
                [ ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]

        AbilityConstructorItem (AbilityConstructor _ detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewNames docZoomMsg detail.info (Category.Type Type.AbilityType))
                [ ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]


view :
    msg
    -> (Reference -> msg)
    -> (Zoom -> msg)
    -> (Id Doc -> msg)
    -> WorkspaceItem
    -> Bool
    -> Html msg
view closeMsg toOpenReferenceMsg toUpdateZoomMsg toggleFoldMsg workspaceItem isFocused =
    let
        focusedAttrs =
            [ classList [ ( "focused", isFocused ) ] ]
    in
    case workspaceItem of
        Loading ref ->
            viewRow ref focusedAttrs [] ( UI.nothing, UI.loadingPlaceholder ) [ ( UI.nothing, UI.loadingPlaceholder ) ]

        Failure ref err ->
            viewClosableRow
                closeMsg
                ref
                focusedAttrs
                (div [ class "error-header" ]
                    [ Icon.view Icon.warn
                    , Icon.view (Reference.toIcon ref)
                    , h3 [ title (Api.errorToString err) ] [ text (HQ.toString (Reference.hashQualified ref)) ]
                    ]
                )
                [ ( UI.nothing
                  , div
                        [ class "error" ]
                        [ text "Unable to load definition: "
                        , span [ class "definition-with-error" ] [ text (Reference.toHumanString ref) ]
                        , text " —  please try again."
                        ]
                  )
                ]

        Success ref data ->
            viewItem closeMsg toOpenReferenceMsg toUpdateZoomMsg toggleFoldMsg ref data isFocused



-- VIEW HELPERS


viewGutter : Html msg -> Html msg
viewGutter content =
    div [ class "gutter" ] [ content ]


viewRow :
    Reference
    -> List (Attribute msg)
    -> List (Html msg)
    -> ( Html msg, Html msg )
    -> List ( Html msg, Html msg )
    -> Html msg
viewRow ref attrs actionsContent ( headerGutter, headerContent ) content =
    let
        headerItems =
            [ viewGutter headerGutter, headerContent ]

        contentRows =
            List.map (\( g, c ) -> div [ class "inner-row" ] [ viewGutter g, c ]) content

        actions =
            if not (List.isEmpty actionsContent) then
                div [ class "actions" ] actionsContent

            else
                UI.nothing
    in
    div
        (class "definition-row" :: id ("definition-" ++ Reference.toString ref) :: attrs)
        [ actions
        , header [ class "inner-row" ] headerItems
        , section [ class "content" ] contentRows
        ]


viewClosableRow :
    msg
    -> Reference
    -> List (Attribute msg)
    -> Html msg
    -> List ( Html msg, Html msg )
    -> Html msg
viewClosableRow closeMsg hash_ attrs header contentItems =
    let
        close =
            a [ class "close", onClick closeMsg ] [ Icon.view Icon.x ]
    in
    viewRow hash_ attrs [ close ] ( UI.nothing, header ) contentItems



-- JSON DECODERS


decodeTypeDetails :
    Decode.Decoder
        { category : TypeCategory
        , name : String
        , otherNames : NEL.Nonempty FQN
        , source : TypeSource
        }
decodeTypeDetails =
    let
        make cat name otherNames source =
            { category = cat, name = name, otherNames = otherNames, source = source }

        decodeTypeDefTag =
            at [ "typeDefinition", "tag" ] Decode.string

        decodeUserObject =
            Decode.map Type.Source (at [ "typeDefinition", "contents" ] Syntax.decode)
    in
    Decode.map4 make
        (Type.decodeTypeCategory "defnTypeTag")
        (field "bestTypeName" Decode.string)
        (field "typeNames" (Util.decodeNonEmptyList FQN.decode))
        (Decode.oneOf
            [ when decodeTypeDefTag ((==) "UserObject") decodeUserObject
            , when decodeTypeDefTag ((==) "BuiltinObject") (Decode.succeed Type.Builtin)
            ]
        )


decodeTypes : Decode.Decoder (List TypeDetailWithDoc)
decodeTypes =
    let
        makeType ( hash_, d ) =
            hash_
                |> Hash.fromString
                |> Maybe.map (\h -> Type h d.category { doc = Nothing, info = Info.makeInfo d.name d.otherNames, source = d.source })

        buildTypes =
            List.map makeType >> MaybeE.values
    in
    Decode.keyValuePairs decodeTypeDetails |> Decode.map buildTypes


decodeTermNamesAndSource :
    Decode.Decoder
        { category : TermCategory
        , name : String
        , otherNames : NEL.Nonempty FQN
        , source : TermSource
        }
decodeTermNamesAndSource =
    let
        make cat name otherNames source =
            { category = cat
            , name = name
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
    Decode.map4 make
        (Term.decodeTermCategory "defnTermTag")
        (field "bestTermName" Decode.string)
        (field "termNames" (Util.decodeNonEmptyList FQN.decode))
        (Decode.oneOf
            [ when decodeTermDefTag ((==) "UserObject") decodeUserObject
            , when decodeTermDefTag ((==) "BuiltinObject") decodeBuiltin
            ]
        )


decodeTerms : Decode.Decoder (List TermDetailWithDoc)
decodeTerms =
    let
        makeTerm ( hash_, d ) =
            hash_
                |> Hash.fromString
                |> Maybe.map (\h -> Term h d.category { doc = Nothing, info = Info.makeInfo d.name d.otherNames, source = d.source })

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
