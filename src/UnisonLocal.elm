module UnisonLocal exposing (..)

import Browser
import Env exposing (Flags)
import UnisonLocal.App as App
import UnisonLocal.PreApp as PreApp


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
