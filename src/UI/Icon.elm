module UI.Icon exposing (..)

import Html exposing (Html, i)
import Html.Attributes exposing (class)


icon : String -> Html msg
icon name =
    i [ class ("icon fas fa-" ++ name) ] []


caretRight : Html msg
caretRight =
    icon "caret-right"


caretDown : Html msg
caretDown =
    icon "caret-down"


namespace : Html msg
namespace =
    icon "box"


term : Html msg
term =
    icon "code"


patch : Html msg
patch =
    icon "directions"


type_ : Html msg
type_ =
    icon "border-none"


ability : Html msg
ability =
    icon "brackets-curly"
