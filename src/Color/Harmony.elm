module Color.Harmony exposing (..)

import Color exposing (Color)
import List.Extra as ListE


harmonizesWith : Color -> List Color
harmonizesWith color =
    let
        complementary_ =
            complementary color

        ( analogousA, analogousB ) =
            analogous color

        ( triadicA, triadicB ) =
            triadic color

        ( splitA, splitB ) =
            splitComplementary color

        ( squareA, squareB, squareC ) =
            square color

        ( tetridicA, tetridicB, tetridicC ) =
            tetridic color

        harmonizesWith_ =
            [ complementary_
            , analogousA
            , analogousB
            , triadicA
            , triadicB
            , splitA
            , splitB
            , squareA
            , squareB
            , squareC
            , tetridicA
            , tetridicB
            , tetridicC
            ]
    in
    ListE.uniqueBy Color.toCssString harmonizesWith_


{-| RGB Difference <https://en.wikipedia.org/wiki/Color_difference> - alpha is disregarded
-}
difference : Color -> Color -> Float
difference a b =
    let
        a_ =
            toRgb255 a

        b_ =
            toRgb255 b

        sum =
            toFloat (((a_.red - b_.red) ^ 2) + ((a_.blue - b_.blue) ^ 2) + ((a_.green - b_.green) ^ 2))
    in
    sqrt sum


toRgb255 : Color -> { red : Int, green : Int, blue : Int }
toRgb255 c =
    let
        rgba =
            Color.toRgba c
    in
    { red = floor (rgba.red * 255)
    , green = floor (rgba.red * 255)
    , blue = floor (rgba.blue * 255)
    }


{-| Opposites on the color wheel
-}
complementary : Color -> Color
complementary color =
    hueAdd 180 color


{-| Adjacent colors on the color wheel
-}
analogous : Color -> ( Color, Color )
analogous color =
    ( hueAdd 30 color
    , hueSubtract 30 color
    )


triadic : Color -> ( Color, Color )
triadic color =
    ( hueAdd 120 color
    , hueAdd 240 color
    )


splitComplementary : Color -> ( Color, Color )
splitComplementary color =
    ( hueAdd 150 color
    , hueAdd 210 color
    )


square : Color -> ( Color, Color, Color )
square color =
    ( hueAdd 90 color
    , hueAdd 180 color
    , hueAdd 270 color
    )


tetridic : Color -> ( Color, Color, Color )
tetridic color =
    ( hueAdd 60 color
    , hueAdd 180 color
    , hueAdd 240 color
    )



-- Internal Helpers


hueAdd : Int -> Color -> Color
hueAdd num color =
    let
        hsla =
            Color.toHsla color

        hue =
            hsla.hue
                |> toAngle
                |> (+) num
                |> wrap360
                |> toPt
    in
    Color.fromHsla { hsla | hue = hue }


hueSubtract : Int -> Color -> Color
hueSubtract num color =
    let
        hsla =
            Color.toHsla color

        hue =
            hsla.hue
                |> toAngle
                |> (-) num
                |> wrap360
                |> toPt
    in
    Color.fromHsla { hsla | hue = hue }


toAngle : Float -> Int
toAngle pt =
    let
        a =
            floor (pt * 360)
    in
    if a > 360 then
        360 - (360 - a)

    else
        a


toPt : Int -> Float
toPt ang =
    toFloat ang / 360


wrap360 : Int -> Int
wrap360 h =
    let
        x =
            modBy 360 h
    in
    if x < 0 then
        x + 360

    else
        x
