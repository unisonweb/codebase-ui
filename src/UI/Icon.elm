module UI.Icon exposing (Icon(..), view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Svg exposing (svg, use)
import Svg.Attributes exposing (height, width, xlinkHref)


type Icon
    = ChevronDown
    | ChevronUp
    | CaretLeft
    | CaretRight
    | CaretUp
    | CaretDown
    | Patch
    | Term
    | Ability
    | Type
    | Document
    | Folder
    | Plus
    | Hash
    | Warn
    | Checkmark
    | X
    | Search


{-| Example: UI.Icon.view UI.Icon.Checkmark |
-}
view : Icon -> Html msg
view icon =
    let
        iconName =
            toIdString icon

        ref =
            spritePath ++ "#icon-" ++ iconName

        className =
            "icon " ++ iconName
    in
    -- Random, its not possible to dynamically set classNames on svg elements
    div [ class className ]
        [ svg [ width "100%", height "100%" ] [ use [ xlinkHref ref ] [] ]
        ]



-- HELPERS


spritePath : String
spritePath =
    "/img/icons.svg"


toIdString : Icon -> String
toIdString icon =
    case icon of
        ChevronDown ->
            "chevron-down"

        ChevronUp ->
            "chevron-up"

        CaretLeft ->
            "caret-left"

        CaretRight ->
            "caret-right"

        CaretUp ->
            "caret-up"

        CaretDown ->
            "caret-down"

        Patch ->
            "patch"

        Term ->
            "term"

        Ability ->
            "ability"

        Type ->
            "type"

        Document ->
            "document"

        Folder ->
            "folder"

        Plus ->
            "plus"

        Hash ->
            "hash"

        Warn ->
            "warn"

        Checkmark ->
            "checkmark"

        X ->
            "x"

        Search ->
            "search"
