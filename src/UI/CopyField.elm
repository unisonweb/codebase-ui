module UI.CopyField exposing (..)

import Html exposing (Html, button, div, input, node, text)
import Html.Attributes exposing (attribute, class, readonly, type_, value)
import UI
import UI.Icon as Icon


type alias CopyField msg =
    { prefix : Maybe String
    , toCopy : String
    , onCopy : String -> msg
    }


copyField : (String -> msg) -> String -> CopyField msg
copyField onCopy toCopy =
    { prefix = Nothing, toCopy = toCopy, onCopy = onCopy }


withPrefix : String -> CopyField msg -> CopyField msg
withPrefix prefix field =
    { field | prefix = Just prefix }


withToCopy : String -> CopyField msg -> CopyField msg
withToCopy toCopy field =
    { field | toCopy = toCopy }


view : CopyField msg -> Html msg
view field =
    let
        prefix =
            field.prefix
                |> Maybe.map (\p -> div [ class "copy-field-prefix" ] [ text p ])
                |> Maybe.withDefault UI.nothing
    in
    div [ class "copy-field" ]
        [ div [ class "copy-field-field" ]
            [ prefix
            , div
                [ class "copy-field-input" ]
                [ input
                    [ type_ "text"
                    , class "copy-field-to-copy"
                    , value field.toCopy
                    , readonly True
                    ]
                    []
                ]
            ]
        , copyButton field.toCopy
        ]



-- HELPERS --------------------------------------------------------------------


{-| We're not using UI.Button here since a click handler is added from
the webcomponent in JS land.
-}
copyButton : String -> Html msg
copyButton toCopy =
    node "copy-on-click"
        [ attribute "text" toCopy ]
        [ button [ class "button contained default" ] [ Icon.view Icon.clipboard ]
        ]
