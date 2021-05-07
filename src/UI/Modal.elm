module UI.Modal exposing
    ( Content(..)
    , Modal
    , modal
    , view
    , withAttributes
    , withHeader
    )

import Html exposing (Attribute, Html, a, div, h2, header, section, text)
import Html.Attributes exposing (class, id, tabindex)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import UI
import UI.Icon as Icon


type Content msg
    = Content (Html msg)
    | CustomContent (Html msg)


type alias Modal msg =
    { id : String
    , closeMsg : msg
    , attributes : List (Attribute msg)
    , header : Maybe (Html msg)
    , content : Content msg
    }


modal : String -> msg -> Content msg -> Modal msg
modal id closeMsg content =
    { id = id
    , closeMsg = closeMsg
    , attributes = []
    , header = Nothing
    , content = content
    }


withHeader : String -> Modal msg -> Modal msg
withHeader title modal_ =
    { modal_ | header = Just (text title) }


withAttributes : List (Attribute msg) -> Modal msg -> Modal msg
withAttributes attrs modal_ =
    { modal_ | attributes = modal_.attributes ++ attrs }


view : Modal msg -> Html msg
view modal_ =
    let
        header_ =
            modal_.header
                |> Maybe.map
                    (\title ->
                        header [ class "modal-header " ]
                            [ h2 [] [ title ]
                            , a [ class "close-modal", onClick modal_.closeMsg ]
                                [ Icon.view Icon.X ]
                            ]
                    )
                |> Maybe.withDefault UI.nothing

        content =
            case modal_.content of
                Content c ->
                    section [ class "modal-content" ] [ c ]

                CustomContent c ->
                    c
    in
    view_ modal_.closeMsg (id modal_.id :: modal_.attributes) [ header_, content ]



-- INTERNALS


view_ : msg -> List (Attribute msg) -> List (Html msg) -> Html msg
view_ closeMsg attrs content =
    div [ id overlayId, on "click" (decodeOverlayClick closeMsg) ]
        [ div (tabindex 0 :: class "modal" :: attrs) content
        ]


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
