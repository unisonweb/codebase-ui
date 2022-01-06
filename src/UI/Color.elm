{- TODO: Autogenerate from a JSON file -}


module UI.Color exposing (..)

import Color as C
import Color.Accessibility exposing (luminance)
import Color.Harmony as Harmony
import List.Extra as ListE


{-| Wraps a more fundamental Color library (avh4/elm-color— a very standard
one from the Elm community). This should never be exposed as we only want
colors from the Unison Design System to be used.
-}
type Color
    = Color C.Color



-- Helpers


toCssString : Color -> String
toCssString (Color color) =
    C.toCssString color


harmonizesWith : Color -> List Color
harmonizesWith ((Color color_) as color) =
    let
        harmonizesWith_ =
            color_
                |> Harmony.harmonizesWith
                |> List.map fromColor_
                |> ListE.uniqueBy toCssString
                |> List.filter (\c -> c /= color)
    in
    harmonizesWith_


{-| Find the closests color in the design system to a full gamut color
-}
fromColor_ : C.Color -> Color
fromColor_ full =
    let
        differences =
            nonGrays
                |> List.map
                    (\((Color c) as color) ->
                        ( Harmony.difference full c, color )
                    )
                |> List.sortBy Tuple.first
    in
    differences
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault grayBase



-- Colors


colors : List Color
colors =
    grays ++ pinks ++ greens ++ blues ++ oranges ++ purples


nonGrays : List Color
nonGrays =
    pinks ++ greens ++ blues ++ oranges ++ purples


dark : List Color
dark =
    colors |> filterLuminance ((>) 0.5)


darkNonGrays : List Color
darkNonGrays =
    nonGrays |> filterLuminance ((>) 0.5)


light : List Color
light =
    colors |> filterLuminance ((<=) 0.5)


lightNonGrays : List Color
lightNonGrays =
    nonGrays |> filterLuminance ((<=) 0.5)


filterLuminance : (Float -> Bool) -> List Color -> List Color
filterLuminance pred colors_ =
    let
        filter_ (Color fullGamut) =
            pred (luminance fullGamut)
    in
    List.filter filter_ colors_



-- Grays


grays : List Color
grays =
    [ grayDarken30
    , grayDarken25
    , grayDarken20
    , grayDarken10
    , grayBase
    , grayLighten20
    , grayLighten30
    , grayLighten40
    , grayLighten45
    , grayLighten50
    , grayLighten55
    , grayLighten60
    , grayLighten100
    ]


grayDarken30 : Color
grayDarken30 =
    Color (C.rgb255 24 24 28)


grayDarken25 : Color
grayDarken25 =
    Color (C.rgb255 34 35 42)


grayDarken20 : Color
grayDarken20 =
    Color (C.rgb255 45 46 53)


grayDarken10 : Color
grayDarken10 =
    Color (C.rgb255 65 66 75)


grayBase : Color
grayBase =
    Color (C.rgb255 81 82 88)


grayLighten20 : Color
grayLighten20 =
    Color (C.rgb255 129 130 134)


grayLighten30 : Color
grayLighten30 =
    Color (C.rgb255 189 191 198)


grayLighten40 : Color
grayLighten40 =
    Color (C.rgb255 209 213 220)


grayLighten45 : Color
grayLighten45 =
    Color (C.rgb255 217 224 231)


grayLighten50 : Color
grayLighten50 =
    Color (C.rgb255 228 234 243)


grayLighten55 : Color
grayLighten55 =
    Color (C.rgb255 241 243 245)


grayLighten60 : Color
grayLighten60 =
    Color (C.rgb255 250 250 251)


grayLighten100 : Color
grayLighten100 =
    Color (C.rgb255 255 255 255)



-- Pinks


pinks : List Color
pinks =
    [ pink1, pink2, pink3 ]


pink1 : Color
pink1 =
    Color (C.rgb255 255 71 86)


pink2 : Color
pink2 =
    Color (C.rgb255 255 108 120)


pink3 : Color
pink3 =
    Color (C.rgb255 255 155 163)



-- Greens


greens : List Color
greens =
    [ green1, green2, green3, green4 ]


green1 : Color
green1 =
    Color (C.rgb255 39 174 96)


green2 : Color
green2 =
    Color (C.rgb255 82 209 136)


green3 : Color
green3 =
    Color (C.rgb255 136 243 181)


green4 : Color
green4 =
    Color (C.rgb255 198 255 222)



-- Blues


blues : List Color
blues =
    [ blue1, blue2, blue3, blue4, blue5 ]


blue1 : Color
blue1 =
    Color (C.rgb255 34 94 190)


blue2 : Color
blue2 =
    Color (C.rgb255 86 149 244)


blue3 : Color
blue3 =
    Color (C.rgb255 158 197 255)


blue4 : Color
blue4 =
    Color (C.rgb255 203 224 255)


blue5 : Color
blue5 =
    Color (C.rgb255 236 242 250)



-- Oranges


oranges : List Color
oranges =
    [ orange1, orange2, orange3, orange4, orange5 ]


orange1 : Color
orange1 =
    Color (C.rgb255 255 136 0)


orange2 : Color
orange2 =
    Color (C.rgb255 255 196 31)


orange3 : Color
orange3 =
    Color (C.rgb255 255 224 139)


orange4 : Color
orange4 =
    Color (C.rgb255 255 238 190)


orange5 : Color
orange5 =
    Color (C.rgb255 255 247 223)



-- Purples


purples : List Color
purples =
    [ purple1, purple2, purple3, purple4 ]


purple1 : Color
purple1 =
    Color (C.rgb255 85 55 123)


purple2 : Color
purple2 =
    Color (C.rgb255 115 77 163)


purple3 : Color
purple3 =
    Color (C.rgb255 154 118 200)


purple4 : Color
purple4 =
    Color (C.rgb255 198 168 236)
