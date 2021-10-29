module UI.Tooltip exposing
    ( Arrow(..)
    , Content(..)
    , MenuItem
    , Position(..)
    , Tooltip
    , menu
    , textMenu
    , tooltip
    , view
    , withArrow
    , withPosition
    )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import UI
import UI.Click as Click exposing (Click)
import UI.Icon as Icon exposing (Icon)


type alias Tooltip msg =
    { arrow : Arrow
    , trigger : Html msg
    , content : Content msg
    , position : Position
    }


type Arrow
    = None
    | Start
    | Middle
    | End


{-| Position relative to trigger
-}
type Position
    = Above
    | Below
    | LeftOf
    | RightOf


type alias MenuItem msg =
    { icon : Maybe (Icon msg), label : String, click : Click msg }


type Content msg
    = Text String
    | Rich (Html msg)
    | Menu (List (MenuItem msg))


menu : List ( Icon msg, String, Click msg ) -> Content msg
menu items =
    items
        |> List.map (\( i, l, c ) -> MenuItem (Just i) l c)
        |> Menu


textMenu : List ( String, Click msg ) -> Content msg
textMenu items =
    items
        |> List.map (\( l, c ) -> MenuItem Nothing l c)
        |> Menu


tooltip : Html msg -> Content msg -> Tooltip msg
tooltip trigger content =
    { arrow = Middle
    , trigger = trigger
    , content = content
    , position = Below
    }


withArrow : Arrow -> Tooltip msg -> Tooltip msg
withArrow arrow tooltip_ =
    { tooltip_ | arrow = arrow }


withPosition : Position -> Tooltip msg -> Tooltip msg
withPosition pos tooltip_ =
    { tooltip_ | position = pos }


view : Tooltip msg -> Html msg
view { arrow, content, trigger, position } =
    let
        viewMenuItem item =
            let
                iconHtml =
                    case item.icon of
                        Just icon ->
                            Icon.view icon

                        Nothing ->
                            UI.nothing
            in
            Click.view [ class "tooltip-menu-item" ] [ iconHtml, text item.label ] item.click

        content_ =
            case content of
                Text t ->
                    text t

                Rich html ->
                    html

                Menu items ->
                    div [ class "tooltip-menu-items" ] (List.map viewMenuItem items)

        tooltip_ =
            -- The tooltip includes a small bridge (made with padding) above
            -- the bubble to allow the user to hover into the tooltip and click
            -- links etc.
            div
                [ class "tooltip"
                , class (positionToClass position)
                , class
                    (arrowToClass arrow)
                , class (contentToClass content)
                ]
                [ div
                    [ class "tooltip-bubble" ]
                    [ content_ ]
                ]
    in
    span [ class "tooltip-trigger" ] [ tooltip_, trigger ]



-- INTERNAL


contentToClass : Content msg -> String
contentToClass content =
    case content of
        Text _ ->
            "content-text"

        Rich _ ->
            "content-rich"

        Menu _ ->
            "content-menu"


positionToClass : Position -> String
positionToClass pos =
    case pos of
        Above ->
            "above"

        Below ->
            "below"

        RightOf ->
            "right-of"

        LeftOf ->
            "left-of"


arrowToClass : Arrow -> String
arrowToClass arrow =
    case arrow of
        None ->
            "arrow-none"

        Start ->
            "arrow-start"

        Middle ->
            "arrow-middle"

        End ->
            "arrow-end"
