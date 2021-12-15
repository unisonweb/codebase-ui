module UI.PageLayout exposing (..)

import Html exposing (Html, div, header, section)
import Html.Attributes exposing (class, classList)
import UI.Sidebar as Sidebar


type PageHero msg
    = PageHero (Html msg)


type PageContent msg
    = PageContent (List (Html msg))


type PageLayout msg
    = HeroLayout
        { hero : PageHero msg
        , content :
            PageContent msg
        }
    | SidebarLayout
        { sidebar : List (Html msg)
        , sidebarToggled : Bool
        , content : PageContent msg
        }
    | FullLayout { content : PageContent msg }



-- VIEW


viewHero : PageHero msg -> Html msg
viewHero (PageHero content) =
    header [ class "page-hero" ] [ content ]


viewContent : PageContent msg -> Html msg
viewContent (PageContent content) =
    section [ class "page-content" ] content


view : PageLayout msg -> Html msg
view page =
    case page of
        HeroLayout { hero, content } ->
            div [ class "page hero-layout" ]
                [ viewHero hero
                , viewContent content
                ]

        SidebarLayout { sidebar, sidebarToggled, content } ->
            div
                [ class "page sidebar-layout"
                , classList [ ( "sidebar-toggled", sidebarToggled ) ]
                ]
                [ Sidebar.view sidebar
                , viewContent content
                ]

        FullLayout { content } ->
            div [ class "page full-layout" ]
                [ viewContent content
                ]
