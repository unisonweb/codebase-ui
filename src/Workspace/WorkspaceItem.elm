module Workspace.WorkspaceItem exposing (..)

import Api
import Definition.AbilityConstructor exposing (AbilityConstructor(..), AbilityConstructorDetail, AbilityConstructorSource(..))
import Definition.Category as Category exposing (Category)
import Definition.DataConstructor exposing (DataConstructor(..), DataConstructorDetail, DataConstructorSource(..))
import Definition.Doc as Doc exposing (Doc(..), DocFoldToggles)
import Definition.Info as Info exposing (Info)
import Definition.Reference as Reference exposing (Reference(..))
import Definition.Source as Source
import Definition.Term as Term exposing (Term(..), TermCategory, TermDetail, TermSignature(..), TermSource(..))
import Definition.Type as Type exposing (Type(..), TypeCategory, TypeDetail, TypeSource(..))
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified as HQ exposing (HashQualified(..))
import Html exposing (Attribute, Html, a, div, h3, header, label, section, span, strong, text)
import Html.Attributes exposing (class, classList, id, title)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (field, index)
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import String.Extra exposing (pluralize)
import UI
import UI.Icon as Icon exposing (Icon)
import UI.Tooltip as Tooltip
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


viewMenuItem : Icon msg -> String -> Html msg
viewMenuItem icon label_ =
    div [ class "menu-item" ] [ Icon.view icon, label [] [ text label_ ] ]


viewMenuItems : Hash -> Info -> Html msg
viewMenuItems hash_ info =
    let
        namespace =
            case info.namespace of
                Just ns ->
                    viewMenuItem Icon.folderOutlined (FQN.toString ns)

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
                        pluralize "other name..." "other names..." numOtherNames
                in
                Tooltip.tooltip (viewMenuItem Icon.tagsOutlined otherNamesLabel) otherNamesTooltipContent
                    |> Tooltip.withArrow Tooltip.TopLeft
                    |> Tooltip.view

            else
                UI.nothing

        formatHash h =
            h |> Hash.toString |> String.dropLeft 1 |> String.left 8

        hash =
            Tooltip.tooltip (viewMenuItem Icon.hash (formatHash hash_)) (text (Hash.toString hash_))
                |> Tooltip.withArrow Tooltip.TopLeft
                |> Tooltip.view
    in
    div [ class "menu-items" ] [ hash, namespace, otherNames ]


viewInfo : msg -> Hash -> Info -> Category -> Html msg
viewInfo onClick_ hash info category =
    div [ class "info" ]
        [ a [ class "toggle-zoom", onClick onClick_ ]
            [ Icon.view Icon.caretRight
            , Icon.view (Category.icon category)
            , h3 [ class "name" ] [ text info.name ]
            ]
        , viewMenuItems hash info
        ]


viewDoc : (Reference -> msg) -> (Doc.FoldId -> msg) -> DocFoldToggles -> Doc -> Html msg
viewDoc toOpenReferenceMsg toggleFoldMsg docFoldToggles doc =
    div [ class "workspace-item-definition-doc" ] [ Doc.view toOpenReferenceMsg toggleFoldMsg docFoldToggles doc ]


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
            div [ class "definition-source" ]
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
    -> (Doc.FoldId -> msg)
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
        TermItem (Term h category detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewInfo docZoomMsg h detail.info (Category.Term category))
                [ ( UI.nothing, viewDoc_ detail.doc )
                , ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]

        TypeItem (Type h category detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewInfo docZoomMsg h detail.info (Category.Type category))
                [ ( UI.nothing, viewDoc_ detail.doc )
                , ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]

        DataConstructorItem (DataConstructor h detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewInfo docZoomMsg h detail.info (Category.Type Type.DataType))
                [ ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]

        AbilityConstructorItem (AbilityConstructor h detail) ->
            viewClosableRow
                closeMsg
                ref
                attrs
                (viewInfo docZoomMsg h detail.info (Category.Type Type.AbilityType))
                [ ( UI.nothing, viewBuiltin data.item )
                , viewSource toOpenReferenceMsg data.item
                ]


view :
    msg
    -> (Reference -> msg)
    -> (Zoom -> msg)
    -> (Doc.FoldId -> msg)
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


decodeDocs : String -> Decode.Decoder (Maybe Doc)
decodeDocs fieldName =
    Decode.oneOf
        [ Decode.map Just (field fieldName (index 0 (index 2 Doc.decode)))
        , Decode.succeed Nothing
        ]


decodeTypeDetails :
    Decode.Decoder
        { category : TypeCategory
        , name : String
        , otherNames : NEL.Nonempty FQN
        , source : TypeSource
        , doc : Maybe Doc
        }
decodeTypeDetails =
    let
        make cat name otherNames source doc =
            { category = cat
            , doc = doc
            , name = name
            , otherNames = otherNames
            , source = source
            }
    in
    Decode.map5 make
        (Type.decodeTypeCategory [ "defnTypeTag" ])
        (field "bestTypeName" Decode.string)
        (field "typeNames" (Util.decodeNonEmptyList FQN.decode))
        (Type.decodeTypeSource [ "typeDefinition", "tag" ] [ "typeDefinition", "contents" ])
        (decodeDocs "typeDocs")


decodeTypes : Decode.Decoder (List TypeDetailWithDoc)
decodeTypes =
    let
        makeType ( hash_, d ) =
            hash_
                |> Hash.fromString
                |> Maybe.map (\h -> Type h d.category { doc = d.doc, info = Info.makeInfo d.name d.otherNames, source = d.source })

        buildTypes =
            List.map makeType >> MaybeE.values
    in
    Decode.keyValuePairs decodeTypeDetails |> Decode.map buildTypes


decodeTermDetails :
    Decode.Decoder
        { category : TermCategory
        , name : String
        , otherNames : NEL.Nonempty FQN
        , source : TermSource
        , doc : Maybe Doc
        }
decodeTermDetails =
    let
        make cat name otherNames source doc =
            { category = cat
            , name = name
            , otherNames = otherNames
            , source = source
            , doc = doc
            }
    in
    Decode.map5 make
        (Term.decodeTermCategory [ "defnTermTag" ])
        (field "bestTermName" Decode.string)
        (field "termNames" (Util.decodeNonEmptyList FQN.decode))
        (Term.decodeTermSource
            [ "termDefinition", "tag" ]
            [ "signature" ]
            [ "termDefinition", "contents" ]
        )
        (decodeDocs "termDocs")


decodeTerms : Decode.Decoder (List TermDetailWithDoc)
decodeTerms =
    let
        makeTerm ( hash_, d ) =
            hash_
                |> Hash.fromString
                |> Maybe.map (\h -> Term h d.category { doc = d.doc, info = Info.makeInfo d.name d.otherNames, source = d.source })

        buildTerms =
            List.map makeTerm >> MaybeE.values
    in
    Decode.keyValuePairs decodeTermDetails |> Decode.map buildTerms


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
