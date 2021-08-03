module UnisonShare exposing (..)

import App
import Browser
import Env exposing (Flags)
import Main


main : Program Flags Main.Model Main.Msg
main =
    Browser.application
        { init = Main.init
        , update = Main.update
        , view = Main.view
        , subscriptions = Main.subscriptions
        , onUrlRequest = App.LinkClicked >> Main.AppMsg
        , onUrlChange = App.UrlChanged >> Main.AppMsg
        }
