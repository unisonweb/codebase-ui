module Hub exposing (..)

import App
import Browser
import Env exposing (Flags)


main : Program Flags App.Model App.Msg
main =
    Browser.application
        { init = App.init
        , update = App.update
        , view = App.view
        , subscriptions = App.subscriptions
        , onUrlRequest = App.LinkClicked
        , onUrlChange = App.UrlChanged
        }
