module UnisonShare exposing (..)

import Browser
import UnisonShare.App as App
import UnisonShare.Env exposing (Flags)
import UnisonShare.PreApp as PreApp


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
