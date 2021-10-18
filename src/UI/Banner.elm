module UI.Banner exposing (..)

import Html exposing (Html, a, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Banner msg
    = Info { content : String }
    | Promotion { promotionId : String, content : String, ctaMsg : msg, ctaLabel : String }


info : String -> Banner msg
info content =
    Info { content = content }


promotion : String -> String -> msg -> String -> Banner msg
promotion promotionId content ctaMsg ctaLabel =
    Promotion
        { promotionId = promotionId
        , content = content
        , ctaMsg = ctaMsg
        , ctaLabel = ctaLabel
        }


view : Banner msg -> Html msg
view banner_ =
    case banner_ of
        Info i ->
            span [ class "banner", class "info" ] [ text i.content ]

        Promotion p ->
            a
                [ class "banner"
                , class "promotion"
                , class p.promotionId
                , onClick p.ctaMsg
                ]
                [ text p.content
                , span [ class "banner-cta" ] [ text p.ctaLabel ]
                ]
