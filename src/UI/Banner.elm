module UI.Banner exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import UI.Click as Click exposing (Click)


type Banner msg
    = Info { content : String }
    | Promotion { promotionId : String, content : String, ctaClick : Click msg, ctaLabel : String }


info : String -> Banner msg
info content =
    Info { content = content }


promotion : String -> String -> Click msg -> String -> Banner msg
promotion promotionId content ctaClick ctaLabel =
    Promotion
        { promotionId = promotionId
        , content = content
        , ctaClick = ctaClick
        , ctaLabel = ctaLabel
        }


view : Banner msg -> Html msg
view banner_ =
    case banner_ of
        Info i ->
            span [ class "banner", class "info" ] [ text i.content ]

        Promotion p ->
            Click.view
                [ class "banner"
                , class "promotion"
                , class p.promotionId
                ]
                [ text p.content
                , span [ class "banner-cta" ] [ text p.ctaLabel ]
                ]
                p.ctaClick
