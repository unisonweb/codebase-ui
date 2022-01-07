module Color.HarmonyTests exposing (..)

import Color exposing (Color, toCssString)
import Color.Harmony as Harmony
import Expect
import Test exposing (..)
import Tuple3 exposing (mapAllThree)


complementary : Test
complementary =
    describe "Color.Harmony.complementary"
        [ test "Returns the complementary color" <|
            \_ ->
                Expect.equal (toCssString (Harmony.complementary mainColor))
                    "rgba(0%,100%,100%,1)"
        ]


analogous : Test
analogous =
    describe "Color.Harmony.analogous"
        [ test "Returns the analogous colors" <|
            \_ ->
                let
                    result =
                        Tuple.mapBoth toCssString toCssString (Harmony.analogous mainColor)
                in
                Expect.equal result ( "rgba(100%,50%,0%,1)", "rgba(100%,50%,0%,1)" )
        ]


triadic : Test
triadic =
    describe "Color.Harmony.triadic"
        [ test "Returns the triadic colors" <|
            \_ ->
                let
                    result =
                        Tuple.mapBoth toCssString toCssString (Harmony.triadic mainColor)
                in
                Expect.equal result ( "rgba(0%,100%,0%,1)", "rgba(0%,0%,100%,1)" )
        ]


splitComplementary : Test
splitComplementary =
    describe "Color.Harmony.splitComplementary"
        [ test "Returns the split complementary colors" <|
            \_ ->
                let
                    result =
                        Tuple.mapBoth toCssString toCssString (Harmony.splitComplementary mainColor)
                in
                Expect.equal result ( "rgba(0%,100%,50%,1)", "rgba(0%,50%,100%,1)" )
        ]


square : Test
square =
    describe "Color.Harmony.square"
        [ test "Returns the square colors" <|
            \_ ->
                let
                    result =
                        mapAllThree toCssString toCssString toCssString (Harmony.square mainColor)
                in
                Expect.equal result ( "rgba(50%,100%,0%,1)", "rgba(0%,100%,100%,1)", "rgba(50%,0%,100%,1)" )
        ]


tetridic : Test
tetridic =
    describe "Color.Harmony.tetridic"
        [ test "Returns the tetridic colors" <|
            \_ ->
                let
                    result =
                        mapAllThree toCssString toCssString toCssString (Harmony.tetridic mainColor)
                in
                Expect.equal result ( "rgba(100%,100%,0%,1)", "rgba(0%,100%,100%,1)", "rgba(0%,0%,100%,1)" )
        ]



-- Helpers


mainColor : Color
mainColor =
    Color.rgb255 255 0 0
