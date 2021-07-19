module TreePathTests exposing (..)

import Expect
import Test exposing (..)
import TreePath


toString : Test
toString =
    describe "TreePath.toString"
        [ test "Returns a string version of a TreePath" <|
            \_ ->
                let
                    path =
                        [ TreePath.VariantIndex 0, TreePath.ListIndex 1, TreePath.VariantIndex 4 ]
                in
                Expect.equal "VariantIndex#0.ListIndex#1.VariantIndex#4" (TreePath.toString path)
        ]
