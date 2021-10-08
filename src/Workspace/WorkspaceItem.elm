module Workspace.WorkspaceItem exposing (..)

import Api
import Definition.AbilityConstructor exposing (AbilityConstructor(..), AbilityConstructorDetail)
import Definition.Category as Category exposing (Category)
import Definition.DataConstructor exposing (DataConstructor(..), DataConstructorDetail)
import Definition.Doc as Doc exposing (Doc, DocFoldToggles)
import Definition.Info as Info exposing (Info)
import Definition.Reference as Reference exposing (Reference)
import Definition.Source as Source
import Definition.Term as Term exposing (Term(..), TermCategory, TermDetail, TermSource)
import Definition.Type as Type exposing (Type(..), TypeCategory, TypeDetail, TypeSource)
import FullyQualifiedName as FQN exposing (FQN)
import Hash exposing (Hash)
import HashQualified as HQ
import Html exposing (Attribute, Html, a, div, h3, header, label, section, span, strong, text)
import Html.Attributes exposing (class, classList, id, title)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (field, index)
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import String.Extra exposing (pluralize)
import UI
import UI.Button as Button
import UI.FoldToggle as FoldToggle
import UI.Icon as Icon exposing (Icon)
import UI.Tooltip as Tooltip
import Util
import Workspace.Zoom as Zoom exposing (Zoom(..))


type WorkspaceItem
    = Loading Reference
    | Failure Reference Http.Error
    | Success Reference ItemData


type DocVisibility
    = Unknown
    | Cropped
    | NotCropped
    | MadeFullyVisible


type alias ItemData =
    { item : Item
    , zoom : Zoom
    , docFoldToggles : DocFoldToggles
    , docVisibility : DocVisibility
    }


type alias WithDoc =
    { doc : Maybe Doc }


type alias TermDetailWithDoc =
    TermDetail WithDoc


type alias TypeDetailWithDoc =
    TypeDetail WithDoc


type Item
    = TermItem TermDetailWithDoc
    | TypeItem TypeDetailWithDoc
      -- TODO: DataConstructorItem and AbilityConstructorItem are currently not
      -- rendered separate from TypeItem
    | DataConstructorItem DataConstructorDetail
    | AbilityConstructorItem AbilityConstructorDetail


{-| WorkspaceItem doesn't manage state itself, but has a limited set of actions
-}
type Msg
    = Close Reference
    | OpenReference Reference Reference
    | UpdateZoom Reference Zoom
    | ToggleDocFold Reference Doc.FoldId
    | ChangePerspectiveToNamespace FQN
    | FindWithinNamespace FQN
    | ShowFullDoc Reference


fromItem : Reference -> Item -> WorkspaceItem
fromItem ref item =
    let
        zoom =
            -- Doc items always have docs
            if isDocItem item then
                Medium

            else if hasDoc item then
                Medium

            else
                Near

        docVisibility =
            if isDocItem item then
                MadeFullyVisible

            else
                Unknown
    in
    Success ref
        { item = item
        , zoom = zoom
        , docFoldToggles = Doc.emptyDocFoldToggles
        , docVisibility = docVisibility
        }


reference : WorkspaceItem -> Reference
reference item =
    case item of
        Loading r ->
            r

        Failure r _ ->
            r

        Success r _ ->
            r


{-| Builtins and Types can't be expanded, so we can skip the Medium Zoom level entirely
TODO: Remove isTypeItem from this conditional when we can collapse types (TypeSummary)
-}
cycleZoom : ItemData -> ItemData
cycleZoom data =
    if isBuiltinItem data.item || isTypeItem data.item || not (hasDoc data.item) then
        { data | zoom = Zoom.cycleEdges data.zoom }

    else
        { data | zoom = Zoom.cycle data.zoom }


isSameReference : WorkspaceItem -> Reference -> Bool
isSameReference item ref =
    reference item == ref


isSameByReference : WorkspaceItem -> WorkspaceItem -> Bool
isSameByReference a b =
    reference a == reference b


isBuiltinItem : Item -> Bool
isBuiltinItem item =
    case item of
        TermItem term ->
            Term.isBuiltin term

        TypeItem type_ ->
            Type.isBuiltin type_

        _ ->
            False


isTypeItem : Item -> Bool
isTypeItem item =
    case item of
        TypeItem _ ->
            True

        _ ->
            False


isTermItem : Item -> Bool
isTermItem item =
    case item of
        TermItem _ ->
            True

        _ ->
            False


isDataConstructorItem : Item -> Bool
isDataConstructorItem item =
    case item of
        DataConstructorItem _ ->
            True

        _ ->
            False


isAbilityConstructorItem : Item -> Bool
isAbilityConstructorItem item =
    case item of
        AbilityConstructorItem _ ->
            True

        _ ->
            False


isDocItem : Item -> Bool
isDocItem item =
    case item of
        TermItem (Term _ Term.DocTerm _) ->
            True

        _ ->
            False


hasDoc : Item -> Bool
hasDoc item =
    let
        hasDoc_ details =
            MaybeE.isJust details.doc
    in
    case item of
        TermItem (Term _ _ d) ->
            hasDoc_ d

        TypeItem (Type _ _ d) ->
            hasDoc_ d

        _ ->
            False



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


viewInfoItem : Icon msg -> String -> Html msg
viewInfoItem icon label_ =
    div [ class "info-item" ] [ Icon.view icon, label [] [ text label_ ] ]


viewInfoItems : Hash -> Info -> Html Msg
viewInfoItems hash_ info =
    let
        namespace =
            case info.namespace of
                Just fqn ->
                    let
                        ns =
                            FQN.toString fqn

                        namespaceMenuItems =
                            [ Tooltip.MenuItem Icon.browse ("Find within " ++ ns) (FindWithinNamespace fqn)
                            , Tooltip.MenuItem Icon.intoFolder ("Change perspective to " ++ ns) (ChangePerspectiveToNamespace fqn)
                            ]
                    in
                    Tooltip.tooltip (viewInfoItem Icon.folderOutlined ns) (Tooltip.Menu namespaceMenuItems)
                        |> Tooltip.withArrow Tooltip.Start
                        |> Tooltip.view

                Nothing ->
                    UI.nothing

        numOtherNames =
            List.length info.otherNames

        otherNames =
            if numOtherNames > 0 then
                let
                    otherNamesTooltipContent =
                        Tooltip.Rich (div [] (List.map (\n -> div [] [ text (FQN.toString n) ]) info.otherNames))

                    otherNamesLabel =
                        pluralize "other name..." "other names..." numOtherNames
                in
                Tooltip.tooltip (viewInfoItem Icon.tagsOutlined otherNamesLabel) otherNamesTooltipContent
                    |> Tooltip.withArrow Tooltip.Start
                    |> Tooltip.view

            else
                UI.nothing

        formattedHash =
            hash_ |> Hash.toShortString |> Hash.stripHashPrefix

        hash =
            Tooltip.tooltip (viewInfoItem Icon.hash formattedHash) (Tooltip.Text (Hash.toString hash_))
                |> Tooltip.withArrow Tooltip.Start
                |> Tooltip.view
    in
    div [ class "info-items" ] [ hash, namespace, otherNames ]


viewInfo : Zoom -> Msg -> Hash -> Info -> Category -> Html Msg
viewInfo zoom onClick_ hash info category =
    div [ class "info" ]
        [ FoldToggle.foldToggle onClick_ |> FoldToggle.isOpen (zoom /= Far) |> FoldToggle.view
        , div [ class "category-icon" ] [ Icon.view (Category.icon category) ]
        , h3 [ class "name" ] [ text info.name ]
        , viewInfoItems hash info
        ]


viewDoc : Reference -> DocVisibility -> DocFoldToggles -> Doc -> Html Msg
viewDoc ref docVisibility docFoldToggles doc =
    let
        ( showFullDoc, shownInFull ) =
            case docVisibility of
                Unknown ->
                    ( UI.nothing, False )

                Cropped ->
                    ( div [ class "show-full-doc" ]
                        [ Button.iconThenLabel (ShowFullDoc ref) Icon.arrowDown "Show full documentation"
                            |> Button.small
                            |> Button.view
                        ]
                    , False
                    )

                _ ->
                    ( UI.nothing, True )

        classes =
            classList
                [ ( "workspace-item-definition-doc", True )
                , ( "shown-in-full", shownInFull )
                ]
    in
    div [ classes ]
        [ div [ class "definition-doc-columns" ]
            [ div [ class "icon-column" ] [ Icon.view Icon.doc ]
            , div
                [ class "doc-column"
                , id ("definition-doc-" ++ Reference.toString ref)
                ]
                [ Doc.view (OpenReference ref)
                    (ToggleDocFold ref)
                    docFoldToggles
                    doc
                ]
            ]
        , showFullDoc
        ]


{-| TODO: Yikes, this isn't great. Needs cleanup
-}
viewSource : Zoom -> Msg -> Source.ViewConfig Msg -> Item -> ( Html msg, Html Msg )
viewSource zoom onSourceToggleClick sourceConfig item =
    let
        viewLineGutter numLines =
            let
                lines =
                    numLines
                        |> List.range 1
                        |> List.map (String.fromInt >> text >> List.singleton >> div [])
            in
            UI.codeBlock [] (div [] lines)

        viewToggableSource foldToggle renderedSource =
            div [ class "definition-source" ]
                [ FoldToggle.view foldToggle, renderedSource ]
    in
    case item of
        TermItem (Term _ _ detail) ->
            let
                ( numLines, source ) =
                    case zoom of
                        Near ->
                            ( Source.numTermLines detail.source
                            , Source.viewTermSource sourceConfig detail.info.name detail.source
                            )

                        _ ->
                            ( Source.numTermSignatureLines detail.source
                            , Source.viewNamedTermSignature sourceConfig detail.info.name (Term.termSignature detail.source)
                            )
            in
            ( numLines, source )
                |> Tuple.mapBoth viewLineGutter (viewToggableSource (FoldToggle.foldToggle onSourceToggleClick |> FoldToggle.isOpen (zoom == Near)))

        TypeItem (Type _ _ detail) ->
            ( detail.source, detail.source )
                |> Tuple.mapBoth Source.numTypeLines (Source.viewTypeSource sourceConfig)
                |> Tuple.mapBoth viewLineGutter (viewToggableSource (FoldToggle.disabled |> FoldToggle.open))

        DataConstructorItem (DataConstructor _ detail) ->
            ( detail.source, detail.source )
                |> Tuple.mapBoth Source.numTypeLines (Source.viewTypeSource sourceConfig)
                |> Tuple.mapBoth viewLineGutter (viewToggableSource (FoldToggle.disabled |> FoldToggle.open))

        AbilityConstructorItem (AbilityConstructor _ detail) ->
            ( detail.source, detail.source )
                |> Tuple.mapBoth Source.numTypeLines (Source.viewTypeSource sourceConfig)
                |> Tuple.mapBoth viewLineGutter (viewToggableSource (FoldToggle.disabled |> FoldToggle.open))


viewItem : Reference -> ItemData -> Bool -> Html Msg
viewItem ref data isFocused =
    let
        ( zoomClass, infoZoomToggle, sourceZoomToggle ) =
            case data.zoom of
                Far ->
                    ( "zoom-level-far", UpdateZoom ref Medium, UpdateZoom ref Near )

                Medium ->
                    ( "zoom-level-medium", UpdateZoom ref Far, UpdateZoom ref Near )

                Near ->
                    ( "zoom-level-near", UpdateZoom ref Far, UpdateZoom ref Medium )

        attrs =
            [ class zoomClass, classList [ ( "focused", isFocused ) ] ]

        sourceConfig =
            Source.Rich (OpenReference ref)

        viewDoc_ doc =
            doc
                |> Maybe.map (viewDoc ref data.docVisibility data.docFoldToggles)
                |> Maybe.withDefault UI.nothing

        viewContent doc =
            [ viewSource data.zoom sourceZoomToggle sourceConfig data.item
            , ( UI.nothing, viewBuiltin data.item )
            , ( UI.nothing, viewDoc_ doc )
            ]

        viewInfo_ hash_ info cat =
            viewInfo data.zoom infoZoomToggle hash_ info cat
    in
    case data.item of
        TermItem (Term h category detail) ->
            viewClosableRow
                ref
                attrs
                (viewInfo_ h detail.info (Category.Term category))
                (viewContent detail.doc)

        TypeItem (Type h category detail) ->
            viewClosableRow
                ref
                attrs
                (viewInfo_ h detail.info (Category.Type category))
                (viewContent detail.doc)

        DataConstructorItem (DataConstructor h detail) ->
            viewClosableRow
                ref
                attrs
                (viewInfo_ h detail.info (Category.Type Type.DataType))
                (viewContent Nothing)

        AbilityConstructorItem (AbilityConstructor h detail) ->
            viewClosableRow
                ref
                attrs
                (viewInfo_ h detail.info (Category.Type Type.AbilityType))
                (viewContent Nothing)


view : WorkspaceItem -> Bool -> Html Msg
view workspaceItem isFocused =
    let
        focusedAttrs =
            [ classList [ ( "focused", isFocused ) ] ]
    in
    case workspaceItem of
        Loading ref ->
            viewRow ref focusedAttrs [] ( UI.nothing, UI.loadingPlaceholder ) [ ( UI.nothing, UI.loadingPlaceholder ) ]

        Failure ref err ->
            viewClosableRow
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
            viewItem ref data isFocused



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
        (class "workspace-item" :: id ("definition-" ++ Reference.toString ref) :: attrs)
        [ actions
        , header [ class "inner-row" ] headerItems
        , section [ class "content" ] contentRows
        ]


viewClosableRow :
    Reference
    -> List (Attribute Msg)
    -> Html Msg
    -> List ( Html Msg, Html Msg )
    -> Html Msg
viewClosableRow ref attrs header contentItems =
    let
        close =
            a [ class "close", onClick (Close ref) ] [ Icon.view Icon.x ]
    in
    viewRow ref attrs [ close ] ( UI.nothing, header ) contentItems



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
