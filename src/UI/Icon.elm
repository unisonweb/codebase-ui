module UI.Icon exposing (Icon(..), custom, view)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Svg exposing (svg, use)
import Svg.Attributes exposing (height, width, xlinkHref)


type Icon
    = UnisonMark
    | ChevronDown
    | ChevronUp
    | CaretLeft
    | CaretRight
    | CaretUp
    | CaretDown
    | Patch
    | Term
    | Ability
    | AbilityConstructor
    | Test
    | Type
    | DataConstructor
    | Doc
    | Folder
    | Plus
    | Hash
    | Warn
    | Checkmark
    | X
    | Search
    | Dot
    | Dash


custom : List (Attribute msg) -> Icon -> Html msg
custom attrs icon =
    let
        iconName =
            toIdString icon

        ref =
            spritePath ++ "#icon-" ++ iconName

        className =
            "icon " ++ iconName
    in
    -- Randomly, its not possible to dynamically set classNames on svg elements
    div (class className :: attrs)
        [ svg [ width "100%", height "100%" ] [ use [ xlinkHref ref ] [] ]
        ]


{-| Example: UI.Icon.view UI.Icon.Checkmark |
-}
view : Icon -> Html msg
view icon =
    custom [] icon



-- HELPERS


spritePath : String
spritePath =
    "require:src/img/icons.svg"


toIdString : Icon -> String
toIdString icon =
    case icon of
        UnisonMark ->
            "unison-mark"

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

        AbilityConstructor ->
            "ability-constructor"

        Type ->
            "type"

        DataConstructor ->
            "data-constructor"

        Test ->
            "test"

        Doc ->
            "doc"

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

        Dot ->
            "dot"

        Dash ->
            "dash"
