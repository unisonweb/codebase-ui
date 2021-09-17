module UI.FoldToggle exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import UI.Icon as Icon


type Position
    = Opened
    | Closed


type Toggleable msg
    = OnToggle msg
    | Disabled


type alias FoldToggle msg =
    { toggleable : Toggleable msg
    , position : Position
    }



-- CREATE


foldToggle : msg -> FoldToggle msg
foldToggle toggleMsg =
    FoldToggle (OnToggle toggleMsg) Closed


disabled : FoldToggle msg
disabled =
    FoldToggle Disabled Closed



-- MODIFY


withPosition : Position -> FoldToggle msg -> FoldToggle msg
withPosition position toggle =
    { toggle | position = position }


withToggleable : Toggleable msg -> FoldToggle msg -> FoldToggle msg
withToggleable toggleable toggle =
    { toggle | toggleable = toggleable }


isOpen : Bool -> FoldToggle msg -> FoldToggle msg
isOpen isOpen_ toggle =
    let
        position =
            if isOpen_ then
                Opened

            else
                Closed
    in
    { toggle | position = position }


open : FoldToggle msg -> FoldToggle msg
open toggle =
    withPosition Opened toggle


isDisabled : Bool -> FoldToggle msg -> FoldToggle msg
isDisabled isDisabled_ toggle =
    if isDisabled_ then
        withToggleable Disabled toggle

    else
        toggle



-- VIEW


view : FoldToggle msg -> Html msg
view toggle =
    let
        isOpen_ =
            toggle.position == Opened

        ( onClickAttrs, isDisabled_ ) =
            case toggle.toggleable of
                OnToggle msg ->
                    ( [ onClick msg ], False )

                Disabled ->
                    ( [], True )
    in
    div
        (classList
            [ ( "fold-toggle", True )
            , ( "folded-open", isOpen_ )
            , ( "disabled", isDisabled_ )
            ]
            :: onClickAttrs
        )
        -- Caret orientation for folded/unfolded is rotated
        -- by CSS such that it can be animated
        [ Icon.view Icon.caretRight ]
