module Code.Hashvatar exposing (..)

import Code.Hash as Hash exposing (Hash)
import Code.Hashvatar.HexGrid as HexGrid
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import List.Extra as ListE
import String.Extra exposing (break)
import UI.Color as Color exposing (Color)


empty : Html msg
empty =
    view_ HexGrid.empty


view : Hash -> Html msg
view hash =
    let
        raw =
            hash |> Hash.toString |> String.replace "#" ""

        numSlots =
            5

        partLength =
            String.length raw // numSlots

        parts =
            break partLength raw

        toCharCodeSum str =
            str
                |> String.toList
                |> List.foldl (\c acc -> acc + Char.toCode c) 0

        grid =
            parts
                |> List.map toCharCodeSum
                |> toGrid
                |> Maybe.withDefault HexGrid.empty
    in
    view_ grid


view_ : HexGrid.HexGrid -> Html msg
view_ grid =
    div [ class "hashvatar" ] [ HexGrid.view grid ]



-- Helpers


getIn : Int -> List Color -> Maybe Color
getIn unmoddedIdx colors_ =
    ListE.getAt (modBy (List.length colors_) unmoddedIdx) colors_


toGrid : List Int -> Maybe HexGrid.HexGrid
toGrid slots =
    let
        selectBg grid_ =
            let
                background =
                    getIn grid_.background Color.darkNonGrays
            in
            Maybe.map
                (\bg ->
                    { background = bg
                    , tendrils = grid_.tendrils
                    , cell1 = grid_.cell1
                    , cell2 = grid_.cell2
                    , cell3 = grid_.cell3
                    }
                )
                background

        selectTendrils grid_ =
            let
                tendrils =
                    getIn grid_.tendrils (Color.inSameHue grid_.background)
            in
            Maybe.map
                (\tr ->
                    { background = grid_.background
                    , tendrils = tr
                    , cell1 = grid_.cell1
                    , cell2 = grid_.cell2
                    , cell3 = grid_.cell3
                    }
                )
                tendrils

        selectCells grid_ =
            Maybe.map3
                (\cell1 cell2 cell3 ->
                    { background = grid_.background
                    , tendrils = grid_.tendrils
                    , cell1 = cell1
                    , cell2 = cell2
                    , cell3 = cell3
                    }
                )
                (getIn grid_.cell1 (Color.harmonizesWith grid_.background))
                (getIn grid_.cell2 (Color.harmonizesWith grid_.background))
                (getIn grid_.cell3 (Color.harmonizesWith grid_.background))
    in
    Maybe.map5
        (\background tendrils cell1 cell2 cell3 ->
            { background = background
            , tendrils = tendrils
            , cell1 = cell1
            , cell2 = cell2
            , cell3 = cell3
            }
        )
        (ListE.getAt 0 slots)
        (ListE.getAt 1 slots)
        (ListE.getAt 2 slots)
        (ListE.getAt 3 slots)
        (ListE.getAt 4 slots)
        |> Maybe.andThen selectBg
        |> Maybe.andThen selectTendrils
        |> Maybe.andThen selectCells
