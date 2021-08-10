module UnisonShare exposing (..)

import App
import Browser
import Env exposing (Flags)
import PreApp


main : Program Flags PreApp.Model PreApp.Msg
main =
    Browser.application
        { init = PreApp.init
        , update = PreApp.update
        , view = PreApp.view
        , subscriptions = PreApp.subscriptions
        , onUrlRequest = App.LinkClicked >> PreApp.AppMsg
        , onUrlChange = App.UrlChanged >> PreApp.AppMsg
        }
