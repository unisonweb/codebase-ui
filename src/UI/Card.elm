module UI.Card exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type alias Card msg =
    { title : Maybe String, items : List (Html msg) }


card : List (Html msg) -> Card msg
card items =
    { title = Nothing, items = items }


titled : String -> List (Html msg) -> Card msg
titled title items =
    { title = Just title, items = items }


withTitle : String -> Card msg -> Card msg
withTitle title card_ =
    { card_ | title = Just title }


withItems : List (Html msg) -> Card msg -> Card msg
withItems items card_ =
    { card_ | items = items }


withItem : Html msg -> Card msg -> Card msg
withItem item card_ =
    { card_ | items = card_.items ++ [ item ] }


view : Card msg -> Html msg
view card_ =
    let
        items =
            case card_.title of
                Just t ->
                    h3 [ class "card-title" ] [ text t ] :: card_.items
    in
    div [ class "card" ] items
