module UI.Modal exposing (view)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (id, tabindex)
import Html.Events exposing (on)
import Json.Decode as Decode


view : msg -> List (Attribute msg) -> List (Html msg) -> Html msg
view closeMsg attrs content =
    div [ id overlayId, on "click" (decodeOverlayClick closeMsg) ]
        [ div (tabindex 0 :: attrs) content
        ]



-- INTERNAL


overlayId : String
overlayId =
    "modal-overlay"


decodeOverlayClick : msg -> Decode.Decoder msg
decodeOverlayClick closeMsg =
    Decode.at [ "target", "id" ] Decode.string
        |> Decode.andThen
            (\c ->
                if String.contains overlayId c then
                    Decode.succeed closeMsg

                else
                    Decode.fail "ignoring"
            )
