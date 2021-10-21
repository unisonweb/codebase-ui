module UIE.ButtonStories exposing (..)

import UI.Button as Button
import UI.Icon as Icon
import UIExplorer exposing (UI, storiesOf)


stories : UI {} () {}
stories =
    storiesOf "Button"
        [ ( "Text"
          , \_ ->
                Button.button () "click me"
                    |> Button.view
          , {}
          )
        , ( "Icon"
          , \_ ->
                Button.icon () Icon.unisonMark
                    |> Button.view
          , {}
          )
        , ( "IconThenLabel"
          , \_ ->
                Button.iconThenLabel () Icon.unisonMark "Unison"
                    |> Button.view
          , {}
          )
        ]
