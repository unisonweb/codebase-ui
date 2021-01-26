module Main exposing (..)

import App
import Browser


main : Program () App.Model App.Msg
main =
    Browser.sandbox
        { init = App.init
        , update = App.update
        , view = App.view
        }
