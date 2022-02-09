module UI.Toolbar exposing (..)

import Html exposing (Html, header)
import Html.Attributes exposing (class)


type alias Toolbar msg =
    { content : Html msg
    }


toolbar : Html msg -> Toolbar msg
toolbar =
    Toolbar


view : Toolbar msg -> Html msg
view { content } =
    header [ class "toolbar" ] [ content ]
